package js7.cluster.watch

import cats.syntax.flatMap.*
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixDeadline
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.watch.ClusterWatch.*
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeLossNotConfirmedProblem, ClusterWatchEventMismatchProblem, ClusterWatchInactiveNodeProblem, NoClusterNodeLostProblem, UntaughtClusterWatchProblem}
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, HasNodes, PassiveLost}
import js7.data.cluster.{ClusterWatchCheckEvent, ClusterWatchRequest}
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
import org.jetbrains.annotations.TestOnly
import scala.util.chaining.scalaUtilChainingOps

final class ClusterWatch(
  now: () => MonixDeadline,
  requireManualNodeLossConfirmation: Boolean = false)
{
  // _state is synchronized
  private var _state: Option[State] = None

  def processRequest(request: ClusterWatchRequest): Checked[Completed] =
    synchronized {
      request.checked >> processRequest2(request)
    }

  private def processRequest2(request: ClusterWatchRequest): Checked[Completed] = {
    import request.{from, clusterState as reportedClusterState}
    val maybeEvent = request match {
      case o: ClusterWatchCheckEvent => Some(o.event)
      case _ => None
    }

    lazy val opString = s"${maybeEvent getOrElse "heartbeat"} --> $reportedClusterState"

    logger.trace(s"$from: $opString${
      _state.fold("")(o => ", after " + o.lastHeartbeat.elapsed.pretty)}")

    val checkedClusterState = (_state, maybeEvent) match {
      case (None, Some(_: ClusterFailedOver)) =>
        Left(UntaughtClusterWatchProblem)

      case (None, _) =>
        logger.info(s"$from teaches clusterState=$reportedClusterState")
        Right(reportedClusterState)

      case (Some(state), _) =>
        state.processRequest(request, opString)
    }

    checkedClusterState match {
      case Left(problem: ClusterNodeLossNotConfirmedProblem) =>
        _state = _state.map(state => state.copy(
          lastHeartbeat = // Update lastHeartbeat only when `from` is active
            Some(state).filter(_.clusterState.activeId != from).fold(now())(_.lastHeartbeat),
          lossRejected = Some(LossRejected(problem.event))))
        Left(problem)

      case Left(problem) =>
        Left(problem)

      case Right(updatedClusterState) =>
        _state = Some(State(
          updatedClusterState,
          lastHeartbeat = now(),
          lossRejected = // Keep lossRejected iff clusterState is unchanged
            _state.filter(_.clusterState == updatedClusterState).flatMap(_.lossRejected),
          requireManualNodeLossConfirmation = requireManualNodeLossConfirmation))
        Right(Completed)
    }
  }

  def confirmNodeLoss(lostNodeId: NodeId): Checked[Unit] =
    synchronized {
      _state
        .toRight(NoClusterNodeLostProblem)
        .flatMap(_.confirmNodeLoss(lostNodeId))
        .map { updated =>
          _state = Some(updated)
        }
    }

  @TestOnly
  private[cluster] def isActive(id: NodeId): Checked[Boolean] =
    unsafeClusterState().map(_.activeId == id)

  def unsafeClusterState(): Checked[HasNodes] =
    _state.toChecked(UntaughtClusterWatchProblem)
      .map(_.clusterState)

  override def toString =
    "ClusterWatch(" +
      _state.fold("untaught")(state =>
        state.clusterState.toShortString + ", " +
          state.lastHeartbeat.elapsed.pretty + " ago") +
      ")"
}

object ClusterWatch
{
  private val logger = Logger(getClass)

  private[ClusterWatch] final case class State(
    clusterState: HasNodes,
    lastHeartbeat: MonixDeadline,
    lossRejected: Option[LossRejected],
    requireManualNodeLossConfirmation: Boolean)
  {
    def processRequest(request: ClusterWatchRequest, opString: => String): Checked[HasNodes] = {
      import request.{from, clusterState as reportedClusterState}
      val maybeEvent = request match {
        case o: ClusterWatchCheckEvent => Some(o.event)
        case _ => None
      }

      def clusterWatchInactiveNodeProblem = ClusterWatchInactiveNodeProblem(
        from, clusterState, lastHeartbeat.elapsed, opString)

      if (clusterState == reportedClusterState) {
        if (maybeEvent.nonEmpty) {
          logger.debug(
            s"$from: Ignore probably duplicate event for already reached clusterState=${
              clusterState
            }")
        }
        Right(reportedClusterState)
      } else
        maybeEvent
          .match_ {
            case Some(_: ClusterSwitchedOver) =>
              // ClusterSwitchedOver is applied by each node and is considered reliable
              Checked.unit

            case Some(ClusterFailedOver(failedActiveId, _, _)) =>
              clusterState match {
                case PassiveLost(setting) if setting.activeId == failedActiveId =>
                  Left(ClusterFailOverWhilePassiveLostProblem)

                case clusterState =>
                  (from == clusterState.passiveId && !isLastHeartbeatStillValid) !!
                    clusterWatchInactiveNodeProblem
              }

            case _ =>
              (from == clusterState.activeId) !! clusterWatchInactiveNodeProblem
          }
          .>>(clusterState
            .applyEvents(maybeEvent.map(NoKey <-: _))
            .match_ {
              case Left(problem) =>
                logger.warn(s"$from: $problem")
                Left(ClusterWatchEventMismatchProblem(
                  maybeEvent, clusterState, reportedClusterState = reportedClusterState))

              case Right(updatedClusterState) =>
                for (event <- maybeEvent) logger.info(s"$from: $event")
                maybeEvent match {
                  case Some(event: ClusterNodeLostEvent)
                    if requireManualNodeLossConfirmation && !isNodeLossConfirmed =>
                    Left(ClusterNodeLossNotConfirmedProblem(event))
                  case _ =>
                    if (updatedClusterState == reportedClusterState) {
                      logger.info(s"$from changes ClusterState to $reportedClusterState")
                      Right(reportedClusterState)
                    } else {
                      // The node may have died just between sending the event to
                      // ClusterWatch and persisting it. Then we have a different state.
                      val previouslyActive = clusterState.activeId.string
                      logger.warn(s"$from forced ClusterState to $reportedClusterState " +
                        s"maybe because heartbeat of up to now active $previouslyActive " +
                        s"is too long ago (${lastHeartbeat.elapsed.pretty})")
                      Right(reportedClusterState)
                    }
                }
            })
          .tap {
            case Left(problem) => logger.warn(s"$from: $problem")
            case Right(_) =>
          }
    }

    def isLastHeartbeatStillValid =
      (lastHeartbeat + clusterState.timing.clusterWatchHeartbeatValidDuration).hasTimeLeft

    def confirmNodeLoss(lostNodeId: NodeId): Checked[State] =
      matchRejectedNodeLostEvent(lostNodeId)
        .toRight(NoClusterNodeLostProblem)
        .map(rejected => copy(
          lossRejected = Some(rejected.copy(
            nodeLossConfirmed = true))))

    private def matchRejectedNodeLostEvent(lostNodeId: NodeId): Option[LossRejected] =
      (clusterState, lossRejected) match {
        case (Coupled(setting), Some(rejected @ LossRejected(ClusterPassiveLost(passiveId), _)))
          if lostNodeId == setting.passiveId && lostNodeId == passiveId =>
          Some(rejected)

        case (Coupled(setting), Some(rejected @ LossRejected(failedOver: ClusterFailedOver, _)))
          if lostNodeId == setting.activeId && lostNodeId == failedOver.lostNodeId =>
          Some(rejected)

        case _ => None
      }

    def isNodeLossConfirmed =
      lossRejected match {
        case Some(LossRejected(_, nodeLossConfirmed)) => nodeLossConfirmed
        case _ => false
      }
  }

  private[ClusterWatch] final case class LossRejected(
    event: ClusterNodeLostEvent,
    nodeLossConfirmed: Boolean = false)
}
