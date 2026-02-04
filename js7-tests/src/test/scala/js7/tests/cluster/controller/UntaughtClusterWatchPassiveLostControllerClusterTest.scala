package js7.tests.cluster.controller

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.cluster.ClusterWatchCounterpart
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost}
import js7.data.cluster.ClusterState.{Coupled, PassiveLost}
import js7.data.cluster.ClusterWatchAskNodeLoss
import js7.data.cluster.ClusterWatchProblems.ClusterNodeIsNotLostProblem
import js7.tester.ScalaTestUtils.awaitAndAssert

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
        awaitAndAssert(clusterWatch.clusterState().exists(_.isInstanceOf[Coupled]))

        // KILL BACKUP
        backupController.terminate(dontNotifyActiveNode = true)
          .await(99.s)
        backupController.stop.await(99.s)
      }

      primaryController.testEventBus
        .whenFilterMap[ClusterWatchCounterpart.TestWaitingForConfirmation, ClusterPassiveLost]:
          _.request match
            case ClusterWatchAskNodeLoss(_, _, _, event: ClusterPassiveLost, _, _) => Some(event)
            case _ => None
        .await(99.s)

      withClusterWatchService() { (clusterWatch, _) =>
        // ClusterWatch is untaught
        assert(clusterWatch.manuallyConfirmNodeLoss(primaryId, "CONFIRMER").await(99.s)
          == Left(ClusterNodeIsNotLostProblem(primaryId, "untaught")))

        // backupId is lost. Wait until active node has detected it.
        awaitAndAssert(
          clusterWatch.manuallyConfirmNodeLoss(backupId, "CONFIRMER").await(99.s)
            != Left(ClusterNodeIsNotLostProblem(backupId, "untaught")))

        primaryController.eventWatch.await[ClusterPassiveLost]()

        awaitAndAssert(
          primaryController.clusterState.await(99.s).isInstanceOf[PassiveLost])

        val ClusterPassiveLost(`backupId`) = primaryController.eventWatch.await[ClusterPassiveLost]()
          .head.value.event: @unchecked

        primaryController.stop.await(99.s)
      }
    }
