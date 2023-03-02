package js7.cluster.watch

import cats.syntax.flatMap.*
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
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

final class ClusterWatch(
  now: () => MonixDeadline,
  requireManualNodeLossConfirmation: Boolean = false,
  val eventBus: ClusterWatchEventBus = new ClusterWatchEventBus)
{
  // Variables are synchronized
  private val _nodeToLossRejected = mutable.Map.empty[NodeId, LossRejected]
  private var _state: Option[State] = None

  def processRequest(request: ClusterWatchRequest): Checked[Confirmed] =
    logger.debugCall("processRequest", request)(
      synchronized {
        request.checked >> processRequest2(request)
      })

  private def processRequest2(request: ClusterWatchRequest): Checked[Confirmed] = {
    import request.{from, clusterState as reportedClusterState}

    lazy val opString = s"${request.maybeEvent.fold("")(o => s"$o --> ")}$reportedClusterState"
    logger.trace(s"$from: $opString${
      _state.fold("")(o => ", after " + o.lastHeartbeat.elapsed.pretty)}")

    val checkedClusterState = (_state, request.maybeEvent) match {
      case (None/*untaught*/, Some(event: ClusterNodeLostEvent)) =>
        manuallyConfirmed(event) match {
          case None => Left(ClusterNodeLossNotConfirmedProblem(event))
          case Some(confirmer) =>
            logger.info(
              s"$from teaches clusterState=$reportedClusterState after ${event.getClass.simpleScalaName} confirmation")
            Right(Some(confirmer) -> reportedClusterState)
        }

      case (None, _) =>
        logger.info(s"$from teaches clusterState=$reportedClusterState")
        Right(None -> reportedClusterState)

      case (Some(state), _) =>
        state.processRequest(request, manuallyConfirmed, opString)
    }

    checkedClusterState match {
      case Left(problem: ClusterNodeLossNotConfirmedProblem) =>
        _nodeToLossRejected(problem.event.lostNodeId) = LossRejected(problem.event)
        _state = _state.map(state => state.copy(
          lastHeartbeat = // Update lastHeartbeat only when `from` is active
            Some(state).filter(_.clusterState.activeId != from).fold(now())(_.lastHeartbeat)))
        eventBus.publish(problem)
        Left(problem)

      case Left(problem) =>
        _nodeToLossRejected.clear()
        Left(problem)

      case Right((maybeManualConfirmer, updatedClusterState)) =>
        _nodeToLossRejected.clear()
        _state = Some(State(
          updatedClusterState,
          lastHeartbeat = now(),
          requireManualNodeLossConfirmation = requireManualNodeLossConfirmation))
        Right(Confirmed(manualConfirmer = maybeManualConfirmer))
    }
  }

  private def manuallyConfirmed(event: ClusterNodeLostEvent): Option[String] =
    _nodeToLossRejected.get(event.lostNodeId)
      .flatMap(_.manuallyConfirmed(event))

  // User manually confirms a ClusterNodeLostEvent event
  def manuallyConfirmNodeLoss(lostNodeId: NodeId, confirmer: String): Checked[Unit] =
    logger.debugCall("manuallyConfirmNodeLoss", lostNodeId)(
      synchronized {
        matchRejectedNodeLostEvent(lostNodeId)
          .toRight(ClusterNodeIsNotLostProblem(lostNodeId))
          .map { lossRejected =>
            _nodeToLossRejected.clear()
            _nodeToLossRejected(lostNodeId) = lossRejected.copy(
              manualConfirmer = Some(confirmer))
          }
      })

  private def matchRejectedNodeLostEvent(lostNodeId: NodeId): Option[LossRejected] =
    (_nodeToLossRejected.get(lostNodeId), _state.map(_.clusterState)) match {
      case (Some(lossRejected), None)
        if lossRejected.event.lostNodeId == lostNodeId =>
        Some(lossRejected)

      case (Some(lossRejected), Some(Coupled(setting)))
        if lossRejected.event.lostNodeId == lostNodeId && lostNodeId == setting.activeId =>
        Some(lossRejected)

      case _ => None
    }

  def clusterNodeLossEventToBeConfirmed(lostNodeId: NodeId): Option[ClusterNodeLostEvent] =
    _nodeToLossRejected.get(lostNodeId)
      //?.filterNot(_.manualConfirmer.isDefined)
      .map(_.event)

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
  private case class LossRejected(
    event: ClusterNodeLostEvent,
    manualConfirmer: Option[String] = None)
  {
    def manuallyConfirmed(event: ClusterNodeLostEvent): Option[String] =
      manualConfirmer.filter(_ => event == this.event)
  }

  private[ClusterWatch] final case class State(
    clusterState: HasNodes,
    lastHeartbeat: MonixDeadline,
    requireManualNodeLossConfirmation: Boolean)
  {
    def processRequest(
      request: ClusterWatchRequest,
      manualConfirmed: ClusterNodeLostEvent => Option[String],
      opString: => String)
    : Checked[(/*manualConfirmer*/Option[String], HasNodes)] = {
      import request.{from, clusterState as reportedClusterState}
      val maybeEvent = request.maybeEvent

      def clusterWatchInactiveNodeProblem = ClusterWatchInactiveNodeProblem(
        from, clusterState, lastHeartbeat.elapsed, opString)

      if (reportedClusterState == clusterState) {
        if (maybeEvent.nonEmpty) {
          logger.debug(
            s"$from: Ignore probably duplicate event for already reached clusterState=$clusterState")
        }
        Right(None -> reportedClusterState)
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
                val confirmer = maybeEvent match {
                  case Some(event: ClusterNodeLostEvent) => manualConfirmed(event)
                  case _ => None
                }
                maybeEvent match {
                  case Some(event: ClusterNodeLostEvent)
                    if requireManualNodeLossConfirmation && !confirmer.isDefined =>
                    Left(ClusterNodeLossNotConfirmedProblem(event))
                  case _ =>
                    if (updatedClusterState == reportedClusterState) {
                      logger.info(s"$from changes ClusterState to $reportedClusterState")
                      Right(confirmer -> reportedClusterState)
                    } else {
                      // The node may have died just between sending the event to
                      // ClusterWatch and persisting it. Then we have a different state.
                      val previouslyActive = clusterState.activeId.string
                      logger.warn(s"$from forced ClusterState to $reportedClusterState " +
                        s"maybe because heartbeat of up to now active $previouslyActive " +
                        s"is too long ago (${lastHeartbeat.elapsed.pretty})")
                      Right(confirmer -> reportedClusterState)
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

  private[watch] final case class Confirmed(manualConfirmer: Option[String] = None)
}
