package js7.tests.cluster.controller

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.cluster.ClusterNode.RestartAfterJournalTruncationException
import js7.cluster.ClusterWatchCounterpart.WaitingForConfirmation
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterPassiveLost}
import js7.data.cluster.ClusterState.Coupled
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem}
import js7.data.cluster.{ClusterWatchCheckEvent, ClusterWatchId}
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.node.NodeId
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.cluster.controller.UntaughtClusterWatchAndFailOverControllerClusterTest.*
import monix.execution.Scheduler.Implicits.traced

// Connection between cluster nodes is broken, leading to ClusterPassiveLost and ClusterFailedOver.
final class UntaughtClusterWatchAndFailOverControllerClusterTest extends ControllerClusterTester:

  protected override def agentPaths = Nil // No Agent needed
  protected override def items = Nil // No Workflow needed

  override protected def primaryControllerConfig = config"""
    # Short timeout because something blocks web server shutdown occasionally
    js7.web.server.shutdown-timeout = 0.5s
    js7.journal.cluster.dont-halt-when-passive-lost-rejected = true # test only
    """.withFallback(super.primaryControllerConfig)

  // Same ClusterWatch restarts while Cluster nodes are decoupled.
  "Disturb primary-backup connection and fail-over" in:
    runMyTest(stopPrimary = false, restartedClusterWatchId = primaryClusterWatchId)

  // FIXME The following three tests do not terminate because ActiveClusterNode is waiting for a
  //  confirmation for ClusterPassiveLost, delaying forever the acknowledment of SnapshotTaken.
  //  A deadlock.

  "Disturb primary-backup connection and fail-over both Cluster node and ClusterWatch" in:
    pending
    runMyTest(stopPrimary = false, restartedClusterWatchId = backupClusterWatchId)

  "Kill active Controller and fail-over" in:
    pending
    runMyTest(stopPrimary = true, restartedClusterWatchId = primaryClusterWatchId)

  "Kill passive Controller and fail-over both Cluster node and ClusterWatch" in:
    pending
    runMyTest(stopPrimary = true, restartedClusterWatchId = backupClusterWatchId)

  private def runMyTest(stopPrimary: Boolean, restartedClusterWatchId: ClusterWatchId) =
    withControllerAndBackup(suppressClusterWatch = true) { (primary, _, backup, _, _) =>
      var primaryController = primary.newController()
      val backupController = backup.newController()

      withClusterWatchService(primaryClusterWatchId) { (cwService, _) =>
        primaryController.eventWatch.await[ClusterCoupled]()
        awaitAndAssert(cwService.clusterState().exists(_.isInstanceOf[Coupled]))
      }

      logger.info("💥 Break connection between cluster nodes 💥")
      sys.props(testAckLossPropertyKey) = "true"
      sys.props(testHeartbeatLossPropertyKey) = "true"

      backupController.testEventBus
        .whenPF[WaitingForConfirmation, Unit](_.request match {
          case ClusterWatchCheckEvent(_, _, `backupId`, _: ClusterFailedOver, _) =>
        })
        .await(99.s)

      if stopPrimary then
        logger.info("💥 Stop Primary Controller 💥")
        primaryController.terminate(clusterAction = Some(ShutDown.ClusterAction.Failover))
          .await(99.s)

      withClusterWatchService(restartedClusterWatchId) { (clusterWatchService, eventBus) =>
        import backupController.eventWatch

        // ClusterWatch is untaught
        val clusterFailedOver = eventBus
          .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterFailedOver] {
            case ClusterNodeLossNotConfirmedProblem(NodeId.backup, event: ClusterFailedOver) => event
          }
          .await(99.s)
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == Some(clusterFailedOver))
        if stopPrimary then
          assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == None)
        else
          val clusterPassiveLost = eventBus
            .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterPassiveLost] {
              case ClusterNodeLossNotConfirmedProblem(NodeId.primary, event: ClusterPassiveLost) => event
            }
            .await(99.s)
          assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        if stopPrimary then
          assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == None)
        else
          val clusterPassiveLost = eventBus
            .whenPF[ClusterNodeLossNotConfirmedProblem, ClusterPassiveLost] {
              case ClusterNodeLossNotConfirmedProblem(NodeId.primary, event: ClusterPassiveLost) => event
            }
            .await(99.s)
          assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == Some(clusterPassiveLost))

        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == Some(clusterFailedOver))

        val eventId = eventWatch.lastAddedEventId
        clusterWatchService.manuallyConfirmNodeLoss(primaryId, "CONFIRMER").await(99.s).orThrow
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(primaryId) == Some(clusterFailedOver))
        assert(clusterWatchService.clusterNodeLossEventToBeConfirmed(backupId) == None)

        val ClusterFailedOver(`primaryId`, `backupId`, _) = eventWatch.await[ClusterFailedOver]()
          .head.value.event: @unchecked

        assert(clusterWatchService.manuallyConfirmNodeLoss(backupId, "CONFIRMER")
          .await(99.s)
          == Left(ClusterNodeIsNotLostProblem(backupId)))

        assert(clusterWatchService.manuallyConfirmNodeLoss(primaryId, "CONFIRMER")
          .await(99.s)
          == Left(ClusterNodeIsNotLostProblem(primaryId)))

        if stopPrimary then
          logger.info("Start Primary Controller")
          sys.props(testAckLossPropertyKey) = "false"
          sys.props(testHeartbeatLossPropertyKey) = "false"
          primaryController = primary.newController()
        else
          // The old previously active cannot recoupled and must be restarted (test only).
          // In production, without js7.journal.cluster.dont-halt-when-passive-lost-rejected,
          // the previously active halts itself to allow a restart.
          primaryController.terminate(clusterAction = Some(ShutDown.ClusterAction.Failover))
            .await(99.s)

          logger.info("Start Primary Controller")
          sys.props(testAckLossPropertyKey) = "false"
          sys.props(testHeartbeatLossPropertyKey) = "false"
          primaryController = primary.newController()
          // Controller restarts due to truncated Journal
          val termination = primaryController.untilTerminated.await(99.s)
          assert(termination.restart)
          intercept[RestartAfterJournalTruncationException] {
            primaryController.stop.await(99.s)
          }

          logger.info("Start Primary Controller again after journal truncation")
          primaryController = primary.newController()
        end if

        eventWatch.await[ClusterCoupled](after = eventId)

        primaryController.terminate().await(99.s)
        backupController.terminate(dontNotifyActiveNode = true).await(99.s)
      }
    }

object UntaughtClusterWatchAndFailOverControllerClusterTest:
  private val logger = Logger[this.type]
  private val primaryClusterWatchId = ClusterWatchId("CLUSTER-WATCH-1")
  private val backupClusterWatchId = ClusterWatchId("CLUSTER-WATCH-2")
