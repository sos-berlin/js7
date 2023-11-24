package js7.tests.cluster.controller

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.cluster.ClusterWatchCounterpart.WaitingForConfirmation
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterPassiveLost}
import js7.data.cluster.ClusterState.Coupled
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem}
import js7.data.cluster.{ClusterWatchCheckEvent, ClusterWatchId}
import js7.data.node.NodeId
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.cluster.controller.UntaughtClusterWatchAndPassiveLostControllerClusterTest.*
import monix.execution.Scheduler.Implicits.traced

// Connection between cluster nodes is broken, leading to ClusterPassiveLost and ClusterFailedOver.
final class UntaughtClusterWatchAndPassiveLostControllerClusterTest extends ControllerClusterTester
{
  protected override def agentPaths = Nil // No Agent needed
  protected override def items = Nil // No Workflow needed

  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  // Same ClusterWatch restarts while Cluster nodes are decoupled.
  "Disturb primary-backup connection" in {
    runMyTest(stopBackup = false, restartedClusterWatchId = primaryClusterWatchId)
  }

  // JS-2092 The other ClusterWatch starts while Cluster nodes are decoupled.
  "Disturb primary-backup connection and fail-over ClusterWatch" in {
    runMyTest(stopBackup = false, restartedClusterWatchId = backupClusterWatchId)
  }

  "Kill passive Controller" in {
    runMyTest(stopBackup = true, restartedClusterWatchId = primaryClusterWatchId)
  }

  // JS-2092 The other ClusterWatch starts while Cluster nodes are decoupled.
  "Kill passive Cluster node and fail-over ClusterWatch" in {
    runMyTest(stopBackup = true, restartedClusterWatchId = backupClusterWatchId)
  }

  private def runMyTest(stopBackup: Boolean, restartedClusterWatchId: ClusterWatchId) =
    withControllerAndBackup(suppressClusterWatch = true) { (primary, _, backup, _, _) =>
      val primaryController = primary.newController()
      var backupController = backup.newController()
      import primaryController.eventWatch

      withClusterWatchService(primaryClusterWatchId) { (cwService, _) =>
        eventWatch.await[ClusterCoupled]()
        awaitAndAssert(cwService.clusterState().exists(_.isInstanceOf[Coupled]))
      }

      logger.info("💥 Break connection between cluster nodes 💥")
      sys.props(testAckLossPropertyKey) = "true"
      sys.props(testHeartbeatLossPropertyKey) = "true"

      primaryController.testEventBus
        .whenPF[WaitingForConfirmation, Unit](_.request match {
          case ClusterWatchCheckEvent(_, _, `primaryId`, _: ClusterPassiveLost, _) =>
        })
        .await(99.s)

      if (stopBackup) {
        logger.info("💥 Stop Backup Controller 💥")
        backupController.terminate(dontNotifyActiveNode = true).await(99.s)
      }

      withClusterWatchService(restartedClusterWatchId) { (clusterWatchService, eventBus) =>
        // ClusterWatch is untaught
        val clusterPassiveLost = eventBus
          .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterPassiveLost] {
            case ClusterNodeLossNotConfirmedProblem(NodeId.primary, event: ClusterPassiveLost) => event
          }
          .await(99.s)
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)

        if (stopBackup) {
          assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)
        } else {
          val clusterFailedOver = eventBus
            .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterFailedOver] {
              case ClusterNodeLossNotConfirmedProblem(NodeId.backup, event: ClusterFailedOver) => event
            }
            .await(99.s)
          assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == Some(clusterFailedOver))
        }

        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        val eventId = eventWatch.lastAddedEventId
        clusterWatchService.manuallyConfirmNodeLoss(backupId, "CONFIRMER").await(99.s).orThrow
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        val ClusterPassiveLost(`backupId`) = eventWatch.await[ClusterPassiveLost]()
          .head.value.event

        assert(clusterWatchService.manuallyConfirmNodeLoss(primaryId, "CONFIRMER")
          .await(99.s)
          == Left(ClusterNodeIsNotLostProblem(primaryId)))

        assert(clusterWatchService.manuallyConfirmNodeLoss(backupId, "CONFIRMER")
          .await(99.s)
          == Left(ClusterNodeIsNotLostProblem(backupId)))

        sys.props(testAckLossPropertyKey) = "false"
        sys.props(testHeartbeatLossPropertyKey) = "false"
        if (stopBackup) {
          logger.info("Start Backup Controller")
          backupController = backup.newController()
        }

        eventWatch.await[ClusterCoupled](after = eventId)

        primaryController.terminate().await(99.s)
        backupController.terminate(dontNotifyActiveNode = true).await(99.s)
      }
    }
}

object UntaughtClusterWatchAndPassiveLostControllerClusterTest {
  private val logger = Logger[this.type]
  private val primaryClusterWatchId = ClusterWatchId("CLUSTER-WATCH-1")
  private val backupClusterWatchId = ClusterWatchId("CLUSTER-WATCH-2")
}
