package js7.tests.cluster.controller

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.cluster.ClusterWatchCounterpart.WaitingForConfirmation
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost}
import js7.data.cluster.ClusterState.{Coupled, PassiveLost}
import js7.data.cluster.ClusterWatchCheckEvent
import js7.data.cluster.ClusterWatchProblems.ClusterNodeIsNotLostProblem
import monix.execution.Scheduler.Implicits.traced

final class UntaughtClusterWatchPassiveLostControllerClusterTest extends ControllerClusterTester:
  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  "PassiveLost" in:
    withControllerAndBackup(suppressClusterWatch = true) { (primary, _, backup, _, _) =>
      val primaryController = primary.newController()
      val backupController = backup.newController()

      withClusterWatchService() { (clusterWatch, _) =>
        primaryController.eventWatch.await[ClusterCoupled]()
        waitForCondition(10.s, 10.ms)(clusterWatch.clusterState().exists(_.isInstanceOf[Coupled]))

        // KILL BACKUP
        backupController.terminate(dontNotifyActiveNode = true)
          .await(99.s)
        backupController.stop.await(99.s)
      }

      primaryController.testEventBus
        .whenFilterMap[WaitingForConfirmation, ClusterPassiveLost](_.request match {
          case ClusterWatchCheckEvent(_, _, _, event: ClusterPassiveLost, _) => Some(event)
          case _ => None
        })
        .await(99.s)

      withClusterWatchService() { (clusterWatch, _) =>
        // ClusterWatch is untaught
        assert(clusterWatch.manuallyConfirmNodeLoss(primaryId, "CONFIRMER").await(99.s)
          == Left(ClusterNodeIsNotLostProblem(primaryId)))

        // backupId is lost. Wait until active node has detected it.
        waitForCondition(99.s, 10.ms)(
          clusterWatch.manuallyConfirmNodeLoss(backupId, "CONFIRMER").await(99.s)
            != Left(ClusterNodeIsNotLostProblem(backupId)))
        assert(clusterWatch.manuallyConfirmNodeLoss(backupId, "CONFIRMER").await(99.s)
          != Left(ClusterNodeIsNotLostProblem(backupId)))

        primaryController.eventWatch.await[ClusterPassiveLost]()

        waitForCondition(10.s, 10.ms)(
          primaryController.clusterState.await(99.s).isInstanceOf[PassiveLost])

        val ClusterPassiveLost(`backupId`) = primaryController.eventWatch.await[ClusterPassiveLost]()
          .head.value.event: @unchecked

        primaryController.stop.await(99.s)
      }
    }
