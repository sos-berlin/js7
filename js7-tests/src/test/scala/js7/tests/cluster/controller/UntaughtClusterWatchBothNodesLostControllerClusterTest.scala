package js7.tests.cluster.controller

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.cluster.ClusterWatchCounterpart.WaitingForConfirmation
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterPassiveLost}
import js7.data.cluster.ClusterState.Coupled
import js7.data.cluster.ClusterWatchCheckEvent
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem}
import js7.data.node.NodeId
import js7.tests.cluster.controller.UntaughtClusterWatchBothNodesLostControllerClusterTest.*
import monix.execution.Scheduler.Implicits.traced
import js7.tester.ScalaTestUtils.awaitAndAssert

// Connection between cluster nodes is broken, leading to ClusterPassiveLost and ClusterFailedOver.
final class UntaughtClusterWatchBothNodesLostControllerClusterTest extends ControllerClusterTester
{
  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  "Failover and recouple" in {
    withControllerAndBackup(suppressClusterWatch = true) { (primary, _, backup, _, _) =>
      val primaryController = primary.newController()
      val backupController = backup.newController()

      withClusterWatchService() { (clusterWatch, _) =>
        primaryController.eventWatch.await[ClusterCoupled]()
        awaitAndAssert(clusterWatch.clusterState().exists(_.isInstanceOf[Coupled]))
      }

      // Suppress acknowledges heartbeat, simulating a connection loss between the cluster nodes
      logger.info("💥 Break connection between cluster nodes 💥")
      sys.props(testAckLossPropertyKey) = "true"
      sys.props(testHeartbeatLossPropertyKey) = "true"

      primaryController.testEventBus
        .whenPF[WaitingForConfirmation, Unit](_.request match {
          case ClusterWatchCheckEvent(_, _, _, _: ClusterPassiveLost, _) =>
        })
        .await(99.s)

      withClusterWatchService() { (clusterWatchService, eventBus) =>
        // ClusterWatch is untaught
        val clusterPassiveLost = eventBus
          .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterPassiveLost] {
            case ClusterNodeLossNotConfirmedProblem(NodeId.primary, event: ClusterPassiveLost) => event
          }
          .await(99.s)
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)

        val clusterFailedOver = eventBus
          .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterFailedOver] {
            case ClusterNodeLossNotConfirmedProblem(NodeId.backup, event: ClusterFailedOver) => event
          }
          .await(99.s)

        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == Some(clusterFailedOver))
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        clusterWatchService.manuallyConfirmNodeLoss(backupId, "CONFIRMER").await(99.s).orThrow
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        val ClusterPassiveLost(`backupId`) = primaryController.eventWatch.await[ClusterPassiveLost]()
          .head.value.event

        assert(clusterWatchService.manuallyConfirmNodeLoss(primaryId, "CONFIRMER")
          .await(99.s)
          == Left(ClusterNodeIsNotLostProblem(primaryId)))

        //? Cluster may have recoupled already:
        //?awaitAndAssert(
        //?  primaryController.clusterState.await(99.s).isInstanceOf[PassiveLost])

        assert(clusterWatchService.manuallyConfirmNodeLoss(primaryId, "CONFIRMER")
          .await(99.s)
          == Left(ClusterNodeIsNotLostProblem(primaryId)))
        assert(clusterWatchService.manuallyConfirmNodeLoss(backupId, "CONFIRMER")
          .await(99.s)
          == Left(ClusterNodeIsNotLostProblem(backupId)))

        primaryController.stop.await(99.s)
        backupController.stop.await(99.s)
      }
    }
  }
}

object UntaughtClusterWatchBothNodesLostControllerClusterTest {
  private val logger = Logger[this.type]
}
