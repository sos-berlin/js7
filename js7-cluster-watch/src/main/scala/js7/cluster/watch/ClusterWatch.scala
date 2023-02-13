package js7.cluster.watch

import cats.syntax.flatMap.*
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.watch.ClusterWatch.*
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem, ClusterWatchEventMismatchProblem, ClusterWatchInactiveNodeProblem, UntaughtClusterWatchProblem}
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, HasNodes, PassiveLost}
import js7.data.cluster.ClusterWatchRequest
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
import org.jetbrains.annotations.TestOnly
import scala.util.chaining.scalaUtilChainingOps

final class ClusterWatch(
  now: () => MonixDeadline,
  requireManualNodeLossConfirmation: Boolean = false,
  val eventBus: ClusterWatchEventBus = new ClusterWatchEventBus)
{
  // Variables are synchronized
  private var _lossRejected: Option[LossRejected] = None
  private var _state: Option[State] = None

  def processRequest(request: ClusterWatchRequest): Checked[Completed] =
    logger.debugCall("processRequest", request)(
      synchronized {
        request.checked >> processRequest2(request)
      })

  private def processRequest2(request: ClusterWatchRequest): Checked[Completed] = {
    import request.{from, clusterState as reportedClusterState}

    lazy val opString = s"${request.maybeEvent getOrElse "heartbeat"} --> $reportedClusterState"
    logger.trace(s"$from: $opString${
      _state.fold("")(o => ", after " + o.lastHeartbeat.elapsed.pretty)}")

    val checkedClusterState = (_state, request.maybeEvent) match {
      case (None/*untaught*/, Some(event: ClusterNodeLostEvent)) =>
        if (!isNodeLossConfirmed(event))
          Left(ClusterNodeLossNotConfirmedProblem(event))
        else {
          logger.info(
            s"$from teaches clusterState=$reportedClusterState after ${event.getClass.simpleScalaName} confirmation")
          Right(reportedClusterState)
        }

      case (None, _) =>
        logger.info(s"$from teaches clusterState=$reportedClusterState")
        Right(reportedClusterState)

      case (Some(state), _) =>
        state.processRequest(request, isNodeLossConfirmed, opString)
    }

    checkedClusterState match {
      case Left(problem: ClusterNodeLossNotConfirmedProblem) =>
        _lossRejected = Some(LossRejected(problem.event))
        _state = _state.map(state => state.copy(
          lastHeartbeat = // Update lastHeartbeat only when `from` is active
            Some(state).filter(_.clusterState.activeId != from).fold(now())(_.lastHeartbeat)))
        eventBus.publish(problem)
        Left(problem)

      case Left(problem) =>
        _lossRejected = None
        Left(problem)

      case Right(updatedClusterState) =>
        _lossRejected = None
        _state = Some(State(
          updatedClusterState,
          lastHeartbeat = now(),
          requireManualNodeLossConfirmation = requireManualNodeLossConfirmation))
        Right(Completed)
    }
  }

  private def isNodeLossConfirmed(event: ClusterNodeLostEvent) =
    _lossRejected.exists(_.isConfirmed(event))

  // User manually confirms a ClusterNodeLostEvent event
  def confirmNodeLoss(lostNodeId: NodeId): Checked[Unit] =
    logger.debugCall("confirmNodeLoss", lostNodeId)(
      synchronized {
        matchRejectedNodeLostEvent(lostNodeId)
          .toRight(ClusterNodeIsNotLostProblem(lostNodeId))
          .map { lossRejected =>
            _lossRejected = Some(lossRejected.copy(nodeLossConfirmed = true))
          }
      })

  private def matchRejectedNodeLostEvent(lostNodeId: NodeId): Option[LossRejected] =
    (_state.map(_.clusterState), _lossRejected) match {
      case (None, Some(lossRejected))
        if lostNodeId == lossRejected.event.lostNodeId =>
        Some(lossRejected)

      case (Some(Coupled(setting)), Some(lossRejected: LossRejected))
        if lostNodeId == setting.activeId && lostNodeId == lossRejected.event.lostNodeId =>
        Some(lossRejected)

      case _ => None
    }

  def clusterNodeLossEventToBeConfirmed(): Option[ClusterNodeLostEvent] =
    _lossRejected.map(_.event)

  @TestOnly
  private[cluster] def isActive(id: NodeId): Checked[Boolean] =
    clusterState().map(_.activeId == id)

  def clusterState(): Checked[HasNodes] =
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

  // ClusterNodeLossEvent has been rejected, but the user may confirm it later
  private case class LossRejected(event: ClusterNodeLostEvent, nodeLossConfirmed: Boolean = false) {
    def isConfirmed(event: ClusterNodeLostEvent) =
      nodeLossConfirmed && event == this.event
  }

  private[ClusterWatch] final case class State(
    clusterState: HasNodes,
    lastHeartbeat: MonixDeadline,
    requireManualNodeLossConfirmation: Boolean)
  {
    def processRequest(
      request: ClusterWatchRequest,
      isNodeLossConfirmed: ClusterNodeLostEvent => Boolean,
      opString: => String)
    : Checked[HasNodes] = {
      import request.{from, clusterState as reportedClusterState}
      val maybeEvent = request.maybeEvent

      def clusterWatchInactiveNodeProblem = ClusterWatchInactiveNodeProblem(
        from, clusterState, lastHeartbeat.elapsed, opString)

      if (reportedClusterState == clusterState) {
        if (maybeEvent.nonEmpty) {
          logger.debug(
            s"$from: Ignore probably duplicate event for already reached clusterState=$clusterState")
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
                    if requireManualNodeLossConfirmation && !isNodeLossConfirmed(event) =>
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

    private def isLastHeartbeatStillValid =
      (lastHeartbeat + clusterState.timing.clusterWatchHeartbeatValidDuration).hasTimeLeft
  }
}
