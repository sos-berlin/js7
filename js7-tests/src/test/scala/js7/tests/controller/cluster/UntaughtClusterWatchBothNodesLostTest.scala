package js7.tests.controller.cluster

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.cluster.ClusterWatchCounterpart.WaitingForConfirmation
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterPassiveLost}
import js7.data.cluster.ClusterState.{Coupled, PassiveLost}
import js7.data.cluster.ClusterWatchCheckEvent
import js7.tests.controller.cluster.UntaughtClusterWatchBothNodesLostTest.*
import monix.execution.Scheduler.Implicits.global

// Connection between cluster nodes is broken, leading to ClusterPassiveLost and ClusterFailedOver.
final class UntaughtClusterWatchBothNodesLostTest extends ControllerClusterTester
{
  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  "Failover and recouple" in {
    withControllerAndBackup(suppressClusterWatch = true) { (primary, backup, _) =>
      val primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s

      withClusterWatchService() { clusterWatch =>
        primaryController.eventWatch.await[ClusterCoupled]()
        waitForCondition(10.s, 10.ms)(clusterWatch.clusterState().exists(_.isInstanceOf[Coupled]))
      }

      // Suppress acknowledges heartbeat, simulating a connection loss between the cluster nodes
      sys.props(testAckLossPropertyKey) = "true"
      sys.props(testHeartbeatLossPropertyKey) = "true"
      logger.info("💥 Connection between cluster nodes is broken 💥")

      primaryController.testEventBus
        .whenPF[WaitingForConfirmation, Unit](_.request match {
          case ClusterWatchCheckEvent(_, _, _, _: ClusterPassiveLost, _) =>
        })
        .await(99.s)

      withClusterWatchService() { clusterWatchService =>
        // ClusterWatch is untaught
        val clusterPassiveLost = clusterWatchService.eventBus
          .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterPassiveLost] {
            case ClusterNodeLossNotConfirmedProblem(event: ClusterPassiveLost) => event
          }
          .await(99.s)
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)

        val clusterFailedOver = clusterWatchService.eventBus
          .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterFailedOver] {
            case ClusterNodeLossNotConfirmedProblem(event: ClusterFailedOver) => event
          }
          .await(99.s)

        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == Some(clusterFailedOver))
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        clusterWatchService.confirmNodeLoss(backupId).orThrow
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        val ClusterPassiveLost(`backupId`) = primaryController.eventWatch.await[ClusterPassiveLost]()
          .head.value.event

        assert(clusterWatchService.confirmNodeLoss(primaryId) == Left(ClusterNodeIsNotLostProblem(primaryId)))

        waitForCondition(10.s, 10.ms)(
          primaryController.clusterState.await(99.s).isInstanceOf[PassiveLost])

        assert(clusterWatchService.confirmNodeLoss(primaryId) == Left(ClusterNodeIsNotLostProblem(primaryId)))
        assert(clusterWatchService.confirmNodeLoss(backupId) == Left(ClusterNodeIsNotLostProblem(backupId)))

        primaryController.terminate() await 99.s
        backupController.terminate() await 99.s
      }
    }
  }
}

object UntaughtClusterWatchBothNodesLostTest {
  private val logger = Logger[this.type]
}
