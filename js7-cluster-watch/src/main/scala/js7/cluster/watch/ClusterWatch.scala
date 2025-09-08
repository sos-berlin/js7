package js7.cluster.watch

import cats.effect.IO
import com.typesafe.scalalogging.Logger as ScalaLogger
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.catsutils.SyncDeadline
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.watch.ClusterWatch.*
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, HasNodes, PassiveLost}
import js7.data.cluster.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem, ClusterWatchEventMismatchProblem, ClusterWatchInactiveNodeProblem, UntaughtClusterWatchProblem}
import js7.data.cluster.ClusterWatchRequest
import js7.data.node.NodeId
import org.jetbrains.annotations.TestOnly
import scala.collection.{View, mutable}
import scala.util.chaining.scalaUtilChainingOps

final class ClusterWatch(
  label: String = "",
  onClusterStateChanged: (HasNodes) => Unit = _ => (),
  requireManualNodeLossConfirmation: Boolean = false,
  onUndecidableClusterNodeLoss: OnUndecidableClusterNodeLoss = _ => IO.unit):

  private val logger = Logger.withPrefix[this.type](label)

  // Variables are synchronized
  private val lock = AsyncLock()
  private val _nodeToLossRejected = mutable.Map.empty[NodeId, LossRejected]
  private var _state: Option[State] = None

  def processRequest(request: ClusterWatchRequest): IO[Checked[Confirmed]] =
    logger.debugIOWithResult("processRequest", request):
      lock.lock:
        IO.pure(request.checked).flatMapT: _ =>
          processRequest2(request)

  private def processRequest2(request: ClusterWatchRequest): IO[Checked[Confirmed]] =
    import request.{from, clusterState as reportedClusterState}

    val checkedClusterState: IO[Checked[(manualConfirmer: Option[String], clusterState: HasNodes)]] =
      SyncDeadline.usingNow: now ?=>
        lazy val opString = s"${request.maybeEvent.fold("")(o => s"$o --> ")}$reportedClusterState"
        logger.trace(s"$from: $opString${
          _state.fold("")(o => ", after " + o.lastHeartbeat.elapsed.pretty)}")

        (_state, request.maybeEvent) match
          case (None/*untaught*/, Some(event: ClusterNodeLostEvent)) if !request.forceWhenUntaught =>
            manuallyConfirmed(event) match
              case None =>
                val problem = ClusterNodeLossNotConfirmedProblem(request.from, event)
                logger.warn:
                  s"ClusterWatch is untaught, therefore unable to confirm a node loss: $problem"
                Left(problem)

              case Some(confirmer) =>
                logger.info(s"$from teaches clusterState=$reportedClusterState after ${
                  event.getClass.simpleScalaName} confirmation")
                Right(Some(confirmer) -> reportedClusterState)

          case (None, _) =>
            logger.info(s"$from teaches clusterState=$reportedClusterState")
            Right(None -> reportedClusterState)

          case (Some(state), _) =>
            state.processRequest(request, manuallyConfirmed, opString)

    checkedClusterState.flatMap:
      case Left(problem: ClusterNodeLossNotConfirmedProblem) =>
        SyncDeadline.usingNow: now ?=>
          _nodeToLossRejected(problem.event.lostNodeId) = LossRejected(problem.event)
          _state = _state.map(state => state.copy(
            lastHeartbeat = // Update lastHeartbeat only when `from` is active
              Some(state).filter(_.clusterState.activeId != from).fold(now)(_.lastHeartbeat)))
        .flatMap: _ =>
          onUndecidableClusterNodeLoss(Some(problem))
            .as(Left(problem))

      case Left(problem) =>
        _nodeToLossRejected.clear()
        IO.left(problem)

      case Right((maybeManualConfirmer, updatedClusterState)) =>
        SyncDeadline.now.flatMap: now =>
          _nodeToLossRejected.clear()
          val changed = !_state.exists(_.clusterState == updatedClusterState)
          _state = Some(State(
            logger,
            updatedClusterState,
            lastHeartbeat = now,
            requireManualNodeLossConfirmation = requireManualNodeLossConfirmation))

          if changed then
            onClusterStateChanged(updatedClusterState)

          maybeManualConfirmer.foldMap: x =>
            onUndecidableClusterNodeLoss(None)
          .as(Right(Confirmed(manualConfirmer = maybeManualConfirmer)))

  private def manuallyConfirmed(event: ClusterNodeLostEvent): Option[String] =
    _nodeToLossRejected.get(event.lostNodeId)
      .flatMap(_.manuallyConfirmed(event))

  // User manually confirms a ClusterNodeLostEvent event
  def manuallyConfirmNodeLoss(lostNodeId: NodeId, confirmer: String): IO[Checked[Unit]] =
    logger.debugIO("manuallyConfirmNodeLoss", lostNodeId):
      lock.lock:
        IO:
          matchRejectedNodeLostEvent(lostNodeId).map: lossRejected =>
            _nodeToLossRejected.clear()
            _nodeToLossRejected(lostNodeId) = lossRejected.copy(
              manualConfirmer = Some(confirmer))

  private def matchRejectedNodeLostEvent(lostNodeId: NodeId): Checked[LossRejected] =
    (_nodeToLossRejected.get(lostNodeId), _state.map(_.clusterState)) match
      case (Some(lossRejected), None)
        if lossRejected.event.lostNodeId == lostNodeId =>
        Right(lossRejected)

      case (Some(lossRejected), Some(Coupled(setting)))
        if lossRejected.event.lostNodeId == lostNodeId && lostNodeId == setting.activeId =>
        Right(lossRejected)

      case (maybeLossRejected, maybeClusterState) =>
        val problem = ClusterNodeIsNotLostProblem(lostNodeId)
        logger.debug:
          s"⚠️  $problem │ ${View(maybeLossRejected, maybeClusterState).flatten.mkString(" ")}"
        Left(problem)

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

  override def toString: String =
    "ClusterWatch(" +
      _state.fold("untaught")(state =>
        state.clusterState.toShortString/* + ", " +
          state.lastHeartbeat.elapsed.pretty + " ago"*/)
      + ")"


object ClusterWatch:
  type OnUndecidableClusterNodeLoss = Option[ClusterNodeLossNotConfirmedProblem] => IO[Unit]

  /** ClusterNodeLossEvent has been rejected, but the user may confirm it later. */
  private case class LossRejected(
    event: ClusterNodeLostEvent,
    manualConfirmer: Option[String] = None):

    def manuallyConfirmed(event: ClusterNodeLostEvent): Option[String] =
      manualConfirmer.filter(_ => event == this.event)


  private[ClusterWatch] final case class State(
    logger: ScalaLogger,
    clusterState: HasNodes,
    lastHeartbeat: SyncDeadline,
    requireManualNodeLossConfirmation: Boolean):

    def processRequest(
      request: ClusterWatchRequest,
      manualConfirmed: ClusterNodeLostEvent => Option[String],
      opString: => String)
      (using now: SyncDeadline.Now)
    : Checked[(manualConfirmer: Option[String], clusterState: HasNodes)] =
      import request.{from, clusterState as reportedClusterState}
      val maybeEvent = request.maybeEvent

      def clusterWatchInactiveNodeProblem =
        ClusterWatchInactiveNodeProblem(from, clusterState, lastHeartbeat.elapsed, opString)

      if reportedClusterState == clusterState then
        if maybeEvent.nonEmpty then
          logger.debug:
            s"$from: Ignore probably duplicate event for already reached clusterState=$clusterState"
        Right(None -> reportedClusterState)
      else
        maybeEvent.match
          case Some(_: ClusterSwitchedOver) =>
            // ClusterSwitchedOver is applied by each node and is considered reliable
            Checked.unit

          case Some(ClusterFailedOver(failedActiveId, _, _)) =>
            clusterState match
              case PassiveLost(setting) if setting.activeId == failedActiveId =>
                Left(ClusterFailOverWhilePassiveLostProblem)

              case clusterState =>
                (from == clusterState.passiveId && !isLastHeartbeatStillValid) !!
                  clusterWatchInactiveNodeProblem

          case _ =>
            (from == clusterState.activeId) !! clusterWatchInactiveNodeProblem
        .flatMap: _ =>
          clusterState.applyEvents(maybeEvent) match
            case Left(problem) =>
              logger.warn(s"$from: $problem")
              Left(ClusterWatchEventMismatchProblem(
                maybeEvent, clusterState, reportedClusterState = reportedClusterState))

            case Right(updatedClusterState) =>
              for event <- maybeEvent do logger.info(s"$from $event")
              val confirmer = maybeEvent match
                case Some(event: ClusterNodeLostEvent) => manualConfirmed(event)
                case _ => None
              maybeEvent match
                case Some(event: ClusterNodeLostEvent)
                  if requireManualNodeLossConfirmation && !confirmer.isDefined =>
                  Left(ClusterNodeLossNotConfirmedProblem(from, event))
                case _ =>
                  if updatedClusterState == reportedClusterState then
                    logger.info(s"$from changes ClusterState to $reportedClusterState")
                    Right(confirmer -> reportedClusterState)
                  else
                    // The node may have died just between sending the event to
                    // ClusterWatch and persisting it. Then we have a different state.
                    val previouslyActive = clusterState.activeId.string
                    logger.warn(s"$from forced ClusterState to $reportedClusterState " +
                      s"maybe because heartbeat of up to now active $previouslyActive " +
                      s"is too long ago (${lastHeartbeat.elapsed.pretty})")
                    Right(confirmer -> reportedClusterState)
        .tap:
          case Left(problem) => logger.warn(s"$from: $problem")
          case Right(_) =>

    private def isLastHeartbeatStillValid(using SyncDeadline.Now) =
      (lastHeartbeat + clusterState.timing.clusterWatchHeartbeatValidDuration).hasTimeLeft

  private[watch] final case class Confirmed(manualConfirmer: Option[String] = None)
