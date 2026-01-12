package js7.tests.cluster.controller

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.cluster.ClusterWatchCounterpart
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterPassiveLost}
import js7.data.cluster.ClusterState.Coupled
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem}
import js7.data.cluster.{ClusterTiming, ClusterWatchCheckEvent, ClusterWatchId}
import js7.data.node.NodeId
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.cluster.controller.UntaughtClusterWatchAndPassiveLostControllerClusterTest.*

// Connection between cluster nodes is broken, leading to ClusterPassiveLost and ClusterFailedOver.
final class UntaughtClusterWatchAndPassiveLostControllerClusterTest extends ControllerClusterTester:

  override protected val clusterTiming = ClusterTiming(heartbeat = 1.s, heartbeatTimeout = 1.s)

  protected override def agentPaths = Nil // No Agent needed
  protected override def items = Nil // No Workflow needed

  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  // Same ClusterWatch restarts while Cluster nodes are decoupled.
  "Disturb primary-backup connection" in:
    runMyTest(stopBackup = false, restartedClusterWatchId = primaryClusterWatchId)

  // JS-2092 The other ClusterWatch starts while Cluster nodes are decoupled.
  "Disturb primary-backup connection and switch ClusterWatch" in:
    runMyTest(stopBackup = false, restartedClusterWatchId = backupClusterWatchId)

  "Kill passive Controller" in:
    runMyTest(stopBackup = true, restartedClusterWatchId = primaryClusterWatchId)

  // JS-2092 The other ClusterWatch starts while Cluster nodes are decoupled.
  "Kill passive Cluster node and switch ClusterWatch" in:
    runMyTest(stopBackup = true, restartedClusterWatchId = backupClusterWatchId)

  private def runMyTest(stopBackup: Boolean, restartedClusterWatchId: ClusterWatchId) =
    withControllerAndBackup(suppressClusterWatch = true) { (primary, _, backup, _, _) =>
      val primaryController = primary.newController()
      var backupController = backup.newController()

      val clusterCoupledConfirmed = primaryController.testEventBus
        .whenPFFuture[ClusterWatchCounterpart.TestConfirmed, Unit]:
          _.request match
            case ClusterWatchCheckEvent(_, _, `primaryId`, _: ClusterCoupled, _, _) =>

      withClusterWatchService(primaryClusterWatchId): (cwService, _) =>
        primaryController.await[ClusterCoupled]()
        awaitAndAssert(cwService.clusterState().exists(_.isInstanceOf[Coupled]))
        clusterCoupledConfirmed.await(99.s)

      logger.info("💥 Break connection between cluster nodes 💥")
      sys.props(testAckLossPropertyKey) = "true"
      sys.props(testHeartbeatLossPropertyKey) = "true"
      sys.props(testSimulateInhibitActivationPropertyKey) = "true"

      primaryController.testEventBus
        .whenPF[ClusterWatchCounterpart.TestWaitingForConfirmation, Unit]:
          _.request match
            case ClusterWatchCheckEvent(_, _, `primaryId`, _: ClusterPassiveLost, _, _) =>
        .await(99.s)

      if stopBackup then
        logger.info("💥 Stop Backup Controller 💥")
        backupController.terminate(dontNotifyActiveNode = true).await(99.s)

      withClusterWatchService(restartedClusterWatchId): (clusterWatchService, eventBus) =>
        // ClusterWatch is untaught
        val clusterPassiveLost = eventBus
          .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterPassiveLost]:
            case ClusterNodeLossNotConfirmedProblem(NodeId.primary, event: ClusterPassiveLost) => event
          .await(99.s)
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)

        if stopBackup then
          assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)
        else
          val clusterFailedOver = eventBus
            .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterFailedOver]:
              case ClusterNodeLossNotConfirmedProblem(NodeId.backup, event: ClusterFailedOver) => event
            .await(99.s)
          assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == Some(clusterFailedOver))

        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        val eventId = primaryController.lastAddedEventId
        clusterWatchService.manuallyConfirmNodeLoss(backupId, "CONFIRMER").await(99.s).orThrow
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == None)
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        val ClusterPassiveLost(`backupId`) = primaryController.await[ClusterPassiveLost]()
          .head.value.event: @unchecked

        assert(clusterWatchService.manuallyConfirmNodeLoss(primaryId, "CONFIRMER")
          .await(99.s)
          == Left(ClusterNodeIsNotLostProblem(primaryId)))

        assert(clusterWatchService.manuallyConfirmNodeLoss(backupId, "CONFIRMER")
          .await(99.s)
          == Left(ClusterNodeIsNotLostProblem(backupId)))

        sys.props(testAckLossPropertyKey) = "false"
        sys.props(testHeartbeatLossPropertyKey) = "false"
        sys.props(testSimulateInhibitActivationPropertyKey) = "false"
        if stopBackup then
          logger.info("Start Backup Controller")
          backupController = backup.newController()

        primaryController.await[ClusterCoupled](after = eventId)

        primaryController.terminate().await(99.s)
        backupController.terminate(dontNotifyActiveNode = true).await(99.s)
    }


object UntaughtClusterWatchAndPassiveLostControllerClusterTest:
  private val logger = Logger[this.type]
  private val primaryClusterWatchId = ClusterWatchId("CLUSTER-WATCH-1")
  private val backupClusterWatchId = ClusterWatchId("CLUSTER-WATCH-2")
