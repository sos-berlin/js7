package js7.tests.cluster.controller

import java.nio.file.Files.size
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.cluster.ClusterWatchCounterpart
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.cluster.ClusterWatchProblems.ClusterNodeIsNotLostProblem
import js7.data.cluster.{ClusterWatchCheckEvent, ClusterWatchId}
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.event.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.NumberValue
import js7.journal.files.JournalFiles.extensions.*
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.cluster.controller.UntaughtClusterWatchFailoverControllerClusterTest.*
import scala.concurrent.duration.Deadline.now

final class UntaughtClusterWatchFailoverControllerClusterTest extends ControllerClusterTester:

  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  "Failover and recouple" in:
    withControllerAndBackup(suppressClusterWatch = true) { (primary, _, backup, _, clusterSetting) =>
      val primaryController = primary.newController()

      val orderId = OrderId("💥")
      var since = now
      var sleepWhileFailing = 0.s
      backup.runController(dontWaitUntilReady = true) { backupController =>
        withClusterWatchService() { (clusterWatch, _) =>
          primaryController.eventWatch.await[ClusterCoupled]()
          awaitAndAssert(clusterWatch.clusterState().exists(_.isInstanceOf[Coupled]))
          // Let ClusterWatch run because it confirms after ClusterCoupled has been committed

          since = now
          sleepWhileFailing = clusterTiming.activeLostTimeout + 3.s

          primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path, arguments = Map(
            "sleep" -> NumberValue(sleepWhileFailing.toSeconds))))
          primaryController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
          backupController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
        }

        // KILL PRIMARY
        primaryController
          .api.executeCommand(ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
          .await(99.s).orThrow
        primaryController.stop.await(99.s)
        logger.info("💥 Controller shut down with backup fail-over while script is running 💥")
        assert(since.elapsed < sleepWhileFailing,
          "— The Controller should have terminated while the shell script runs")

        backupController.testEventBus
          .whenFilterMap[ClusterWatchCounterpart.TestWaitingForConfirmation, ClusterFailedOver]:
            _.request match 
              case ClusterWatchCheckEvent(_, _, _, event: ClusterFailedOver, _, _) => Some(event)
              case _ => None
          .await(99.s)

        withClusterWatchService(ClusterWatchId("CLUSTER-WATCH-2")) { (clusterWatchService, _) =>
            // ClusterWatch is untaught
            // backupId ist not lost
          assert(clusterWatchService
            .manuallyConfirmNodeLoss(backupId, "CONFIRMER")
            .await(99.s)
            == Left(ClusterNodeIsNotLostProblem(backupId)))

          // primaryId is lost. Wait until passive node has detected it.
          awaitAndAssert(
            clusterWatchService
              .manuallyConfirmNodeLoss(primaryId, "CONFIRMER")
              .await(99.s)
              != Left(ClusterNodeIsNotLostProblem(primaryId)))
          clusterWatchService
            .manuallyConfirmNodeLoss(primaryId, "CONFIRMER")
            .await(99.s).orThrow

          val Stamped(failedOverEventId, _, NoKey <-: clusterFailedOver) =
            backupController.eventWatch.await[ClusterFailedOver]().head
          assert(clusterFailedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.last ||
                 clusterFailedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.dropRight(1).last)
          val expectedFailedFile = primaryController.conf.journalLocation
            .file(clusterFailedOver.failedAt.fileEventId)
          assert(clusterFailedOver.failedAt.position == size(expectedFailedFile))

          val registered = backupController.eventWatch.await[ClusterWatchRegistered](
            after = failedOverEventId)
          assert(registered.head.value.event
            == ClusterWatchRegistered(clusterWatchService.clusterWatchId))

          assert(clusterWatchService
            .manuallyConfirmNodeLoss(backupId, "CONFIRMER")
            .await(99.s)
            == Left(ClusterNodeIsNotLostProblem(backupId)))

          val expectedFailedOver = FailedOver(
            clusterSetting.copy(
              activeId = backupId,
              clusterWatchId = Some(clusterWatchService.clusterWatchId)),
            clusterFailedOver.failedAt)
          // Why wait ???
          awaitAndAssert(backupController.clusterState.await(99.s) == expectedFailedOver)

          backupController.eventWatch.await[OrderFinished](_.key == orderId, after = failedOverEventId)
        }
      }
    }


object UntaughtClusterWatchFailoverControllerClusterTest:
  private val logger = Logger[this.type]
