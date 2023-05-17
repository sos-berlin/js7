package js7.tests.cluster.controller

import java.nio.file.Files.size
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.cluster.ClusterWatchCounterpart.WaitingForConfirmation
import js7.cluster.watch.api.ClusterWatchProblems.ClusterNodeIsNotLostProblem
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.cluster.{ClusterWatchCheckEvent, ClusterWatchId}
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.event.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.NumberValue
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.cluster.controller.UntaughtClusterWatchFailoverControllerClusterTest.*
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.Deadline.now

final class UntaughtClusterWatchFailoverControllerClusterTest extends ControllerClusterTester
{
  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  "Failover and recouple" in {
    withControllerAndBackup(suppressClusterWatch = true) { (primary, _, backup, _, clusterSetting) =>
      val primaryController = primary.newController(httpPort = Some(primaryControllerPort))

      backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
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
          .api.executeCommand(ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
          .await(99.s).orThrow
        primaryController.stop.await(99.s)
        logger.info("💥 Controller shut down with backup fail-over while script is running 💥")
        assert(since.elapsed < sleepWhileFailing,
          "— The Controller should have terminated while the shell script runs")

        backupController.testEventBus
          .whenFilterMap[WaitingForConfirmation, ClusterFailedOver](_.request match {
            case ClusterWatchCheckEvent(_, _, _, event: ClusterFailedOver, _) => Some(event)
            case _ => None
          })
          .await(99.s)

        withClusterWatchService(ClusterWatchId("CLUSTER-WATCH-2")) { clusterWatchService =>
            // ClusterWatch is untaught
            // backupId ist not lost
          assert(clusterWatchService.manuallyConfirmNodeLoss(backupId, "CONFIRMER")
            == Left(ClusterNodeIsNotLostProblem(backupId)))

          // primaryId is lost. Wait until passive node has detected it.
          waitForCondition(99.s, 10.ms)(
          clusterWatchService.manuallyConfirmNodeLoss(primaryId, "CONFIRMER")
            != Left(ClusterNodeIsNotLostProblem(primaryId)))
          clusterWatchService.manuallyConfirmNodeLoss(primaryId, "CONFIRMER").orThrow

          val Stamped(failedOverEventId, _, NoKey <-: clusterFailedOver) =
            backupController.eventWatch.await[ClusterFailedOver]().head
          assert(clusterFailedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.last ||
                 clusterFailedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.dropRight(1).last)
          val expectedFailedFile = primaryController.conf.journalMeta
            .file(clusterFailedOver.failedAt.fileEventId)
          assert(clusterFailedOver.failedAt.position == size(expectedFailedFile))

          val registered = backupController.eventWatch.await[ClusterWatchRegistered](
            after = failedOverEventId)
          assert(registered.head.value.event
            == ClusterWatchRegistered(clusterWatchService.clusterWatchId))

          assert(clusterWatchService.manuallyConfirmNodeLoss(backupId, "CONFIRMER")
            == Left(ClusterNodeIsNotLostProblem(backupId)))

          sleep(100.ms) // Why ???
          assert(backupController.clusterState.await(99.s) ==
            FailedOver(
              clusterSetting.copy(
                activeId = backupId,
                clusterWatchId = Some(clusterWatchService.clusterWatchId)),
              clusterFailedOver.failedAt))

          backupController.eventWatch.await[OrderFinished](_.key == orderId, after = failedOverEventId)
        }
      }
    }
  }
}

object UntaughtClusterWatchFailoverControllerClusterTest {
  private val logger = Logger[this.type]
}
