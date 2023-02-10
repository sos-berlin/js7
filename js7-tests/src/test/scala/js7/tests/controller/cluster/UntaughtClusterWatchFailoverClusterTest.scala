package js7.tests.controller.cluster

import java.nio.file.Files.size
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.cluster.ClusterWatchCounterpart.WaitingForConfirmation
import js7.cluster.watch.api.ClusterWatchProblems.ClusterNodeIsNotLostProblem
import js7.common.guice.GuiceImplicits.RichInjector
import js7.controller.configuration.ControllerConfiguration
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver}
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.cluster.ClusterWatchCheckEvent
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.event.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.NumberValue
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.tests.controller.cluster.ControllerClusterTester.*
import js7.tests.controller.cluster.UntaughtClusterWatchFailoverClusterTest.*
import js7.tests.testenv.ControllerClusterForScalaTest.clusterWatchId
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.Deadline.now

final class UntaughtClusterWatchFailoverClusterTest extends ControllerClusterTester
{
  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  "Failover and recouple" in {
    withControllerAndBackup(suppressClusterWatch = true) { (primary, backup, clusterSetting) =>
      val primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s

      withClusterWatchService() { clusterWatch =>
        primaryController.eventWatch.await[ClusterCoupled]()
        waitForCondition(10.s, 10.ms)(clusterWatch.clusterState().exists(_.isInstanceOf[Coupled]))
      }

      val since = now
      val sleepWhileFailing = clusterTiming.activeLostTimeout + 1.s

      val orderId = OrderId("💥")
      primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path, arguments = Map(
        "sleep" -> NumberValue(sleepWhileFailing.toSeconds))))
      primaryController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      backupController.eventWatch.await[OrderProcessingStarted](_.key == orderId)

      // KILL PRIMARY
      primaryController
        .executeCommandAsSystemUser(ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
        .await(99.s).orThrow
      primaryController.terminated await 99.s
      logger.info("💥 Controller shut down with backup fail-over while script is running 💥")
      assert(since.elapsed < sleepWhileFailing,
        "— The Controller should have terminated while the shell script runs")

      backupController.testEventBus
        .whenFilterMap[WaitingForConfirmation, ClusterFailedOver](_.request match {
          case ClusterWatchCheckEvent(_, _, _, event: ClusterFailedOver, _) => Some(event)
          case _ => None
        })
        .await(99.s)

      withClusterWatchService() { clusterWatchService =>
        // backupId ist not lost
        assert(clusterWatchService.confirmNodeLoss(backupId)
          == Left(ClusterNodeIsNotLostProblem(backupId)))

        // primaryId is lost. Wait until passive node has detected it.
        waitForCondition(99.s, 10.ms)(
          clusterWatchService.confirmNodeLoss(primaryId)
            != Left(ClusterNodeIsNotLostProblem(primaryId)))
        clusterWatchService.confirmNodeLoss(primaryId).orThrow

        val Stamped(failedOverEventId, _, NoKey <-: failedOver) =
          backupController.eventWatch.await[ClusterFailedOver]().head
        assert(failedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.last ||
               failedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.dropRight(1).last)
        val expectedFailedFile = primaryController.injector.instance[ControllerConfiguration]
          .journalMeta.file(failedOver.failedAt.fileEventId)
        assert(failedOver.failedAt.position == size(expectedFailedFile))

        waitForCondition(10.s, 10.ms)(backupController.clusterState.await(99.s).isInstanceOf[FailedOver])
        assert(backupController.clusterState.await(99.s) ==
          FailedOver(
            clusterSetting.copy(activeId = backupId, clusterWatchId = Some(clusterWatchId)),
            failedOver.failedAt))

        backupController.eventWatch.await[OrderFinished](_.key == orderId, after = failedOverEventId)
      }

      primaryController.terminate() await 99.s
      backupController.terminate() await 99.s
    }
  }
}

object UntaughtClusterWatchFailoverClusterTest {
  private val logger = Logger[this.type]
}
