package js7.cluster.watch

import cats.effect.IO
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.catsutils.SyncDeadline
import js7.base.catsutils.SyncDeadline.Now
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked.RichCheckedF
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.watch.ClusterWatch.*
import js7.data.cluster.ClusterEvent.ClusterNodeLostEvent
import js7.data.cluster.ClusterState.{Coupled, HasNodes}
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem, ClusterWatchNotAskingProblem, UntaughtClusterWatchProblem}
import js7.data.cluster.{ClusterWatchAskNodeLoss, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchCommitNodeLoss, ClusterWatchNonCommitRequest, ClusterWatchRequest, Confirmer}
import js7.data.node.NodeId
import org.jetbrains.annotations.TestOnly
import scala.collection.{View, mutable}

final class ClusterWatch(
  protected val label: String = "",
  onClusterStateChanged: HasNodes => Unit = _ => (),
  protected val requireManualNodeLossConfirmation: Boolean = false,
  onUndecidableClusterNodeLoss: OnUndecidableClusterNodeLoss = _ => IO.unit)
extends ClusterWatchStateMixin:

  private val logger = Logger.withPrefix[this.type](label)

  // Variables are synchronized
  private val lock = AsyncLock()
  private val _nodeToLossRejected = mutable.Map.empty[NodeId, LossRejected]
  private var _state: State = Untaught

  def processRequest(request: ClusterWatchRequest): IO[Checked[Confirmed]] =
    logger.debugIOWithResult(s"processRequest($request)"):
      lock.lock:
        IO.pure(request.checked).flatMapT: _ =>
          processRequest2(request)
            .onProblem: problem =>
              IO(logger.warn(problem.toString))

  private def processRequest2(request: ClusterWatchRequest): IO[Checked[Confirmed]] =
    import request.{from, clusterState as reportedClusterState}
    SyncDeadline.usingNow:
      lazy val opString = s"${request.maybeEvent.fold("")(o => s"$o --> ")}$reportedClusterState"
      logger.trace(s"$from: $opString${
        _state.ifNormal.fold("")(o => ", after " + o.lastHeartbeat.elapsed.pretty)}")

      _state.ifSubtype[AskingNodeLoss].fold(Checked.unit): state =>
        checkAndMaybeResetState(state, request)
      .flatMap: _ =>
        (request, _state) match
          case (request: (ClusterWatchCheckEvent | ClusterWatchAskNodeLoss), Untaught) =>
            checkIfConfirmed(request)

          case (request: ClusterWatchCheckState, Untaught) =>
            logger.info(s"$from teaches clusterState=$reportedClusterState")
            Right(None)

          case (request: ClusterWatchCommitNodeLoss, _: (Untaught | Normal)) =>
            Left(ClusterWatchNotAskingProblem)

          case (request: ClusterWatchNonCommitRequest, state: Normal) =>
            state.processRequest(request, ifManuallyConfirmed, opString)

          case (_: ClusterWatchNonCommitRequest, _: AskingNodeLoss) =>
            // checkAndMaybeResetState avoids this case
            Left(Problem(s"${request.toShortString} is not appropriate for $_state"))

          case (request: ClusterWatchCommitNodeLoss, state: AskingNodeLoss) =>
            // checkAndMaybeResetState has handled timeout
            state.processRequest(request, ifManuallyConfirmed(request.event), opString)
    .onProblem:
      case problem: ClusterNodeLossNotConfirmedProblem =>
        SyncDeadline.usingNow: now ?=>
          _nodeToLossRejected(problem.event.lostNodeId) = LossRejected(problem.event)
          _state match
            case state: Normal =>
              if from == state.clusterState.activeId then
                _state = state.copy(lastHeartbeat = now)
            case _ =>
        .flatMap: _ =>
          onUndecidableClusterNodeLoss(Some(problem))
      case _ => IO.unit
    .flatMapT: maybeManualConfirmer =>
      (request, _state) match
        case (request: (ClusterWatchCheckState | ClusterWatchCheckEvent), state: (Untaught | Normal)) =>
          _nodeToLossRejected.clear()
          processStandard(state, request, maybeManualConfirmer)

        case (request: ClusterWatchAskNodeLoss, state: Normal) =>
          processWatchAskNodeLoss(state, request, maybeManualConfirmer)

        case (request: ClusterWatchAskNodeLoss, state: Untaught) if maybeManualConfirmer.isDefined =>
          processWatchAskNodeLoss(state, request, maybeManualConfirmer)

        case (request: ClusterWatchCommitNodeLoss, state: AskingNodeLoss) =>
          _nodeToLossRejected.clear()
          processCommitNodeLoss(state, request, maybeManualConfirmer)

        case (request: ClusterWatchCommitNodeLoss, _) =>
          logger.warn(s"${request.toShortString
            }: not asking, timed-out, or active cluster node sent a heartbeat • $_state")
          IO.left(ClusterWatchNotAskingProblem)

        case (_, Untaught) =>
          IO.left(UntaughtClusterWatchProblem)

        case _ =>
          IO.left(Problem(s"ClusterWatch: Unexpected $_state"))

  private def checkAndMaybeResetState(state: AskingNodeLoss, request: ClusterWatchRequest)
    (using Now)
  : Checked[Unit] =
    if state.holdUntil.hasElapsed then
      logger.warn(s"AskingNodeLoss is no longer valid")
      setState(state.restoreNormal)
      Checked.unit
    else if !request.isSubtypeOf[ClusterWatchCommitNodeLoss] then
      setState(state.restoreNormal)
      Left(ClusterWatchNotAskingProblem)
    else
      Checked.unit

  private def processStandard(
    state: Untaught | Normal,
    request: ClusterWatchCheckState | ClusterWatchCheckEvent,
    maybeManualConfirmer: Option[Confirmer])
  : IO[Checked[Confirmed]] =
    SyncDeadline.now.flatMap: now =>
      _nodeToLossRejected.clear()
      val changed = !state.ifNormal.exists:
        _.clusterState == request.clusterState
      setState(Normal(
        request.clusterState,
        lastHeartbeat = now,
        requireManualNodeLossConfirmation = requireManualNodeLossConfirmation))
      if changed then
        logger.debug(s"_state = $stateString")
        onClusterStateChanged(request.clusterState)

      maybeManualConfirmer.foldMap: _ =>
        onUndecidableClusterNodeLoss(None)
      .as(Right(Confirmed(manualConfirmer = maybeManualConfirmer)))

  private def processWatchAskNodeLoss(
    state: Untaught | Normal,
    request: ClusterWatchAskNodeLoss,
    maybeManualConfirmer: Option[Confirmer])
  : IO[Checked[Confirmed]] =
    SyncDeadline.now.map: now =>
      setState(AskingNodeLoss(
        clusterState = request.clusterState,
        requireManualNodeLossConfirmation = requireManualNodeLossConfirmation,
        request = request,
        askedClusterState = request.clusterState,
        lastHeartbeat = now,
        holdUntil = now + request.hold,
        savedNormal = state))
      Right(Confirmed(maybeManualConfirmer))

  private def processCommitNodeLoss(
    state: AskingNodeLoss,
    request: ClusterWatchCommitNodeLoss,
    maybeManualConfirmer: Option[Confirmer])
  : IO[Checked[Confirmed]] =
    SyncDeadline.now.flatMap: now =>
      if state.holdUntil <= now then // DUPLICATE ???
        logger.warn(s"${request.toShortString} expires")
        setState(state.restoreNormal)
        IO.left(ClusterWatchNotAskingProblem) // Client should ask again
      else if request.clusterState != state.askedClusterState  then
        logger.warn(s"${request.toShortString}: ${request.clusterState.toShortString
          } does not match previously asked ${state.askedClusterState.toShortString}")
        IO.left(ClusterWatchNotAskingProblem)
      else if request.event != state.request.event then
        logger.warn(s"${request.toShortString}: ${request.event
          } does not match previously asked ${state.request.event}")
        IO.left(ClusterWatchNotAskingProblem)
      else
        _nodeToLossRejected.clear()
        val changed = !state.ifNormal.exists(_.clusterState == request.clusterState)
        setState(Normal(
          request.clusterState,
          lastHeartbeat = now,
          requireManualNodeLossConfirmation = requireManualNodeLossConfirmation))

        if changed then
          onClusterStateChanged(request.clusterState)

        maybeManualConfirmer.foldMap: _ =>
          onUndecidableClusterNodeLoss(None)
        .as(Right(Confirmed(maybeManualConfirmer)))

  private def setState(newState: State): Unit =
    if _state != newState then
      logger.trace(s"_state=$newState")
      (_state, newState) match
        case (Untaught, newState) =>
        case (oldState: Normal, newState: Normal) =>
          if oldState.clusterState != newState.clusterState then
            logger.debug(s"state=$newState")
        case (oldState: AskingNodeLoss, newState: Normal) =>
          logger.debug(s"state=$newState")
        case _ =>
      _state = newState
    end if

  private def checkIfConfirmed(request: ClusterWatchCheckEvent | ClusterWatchAskNodeLoss)
  : Checked[Option[Confirmer]] =
    request.event match
      case event: ClusterNodeLostEvent if !request.forceWhenUntaught =>
        ifManuallyConfirmed(event) match
          case None =>
            val problem = ClusterNodeLossNotConfirmedProblem(request.from, event)
            logger.warn:
              s"ClusterWatch is untaught, therefore unable to confirm a node loss: $problem"
            Left(problem)

          case Some(confirmer) =>
            logger.info(s"${request.from} teaches clusterState=${request.clusterState
              } after $confirmer confirmed, that ${event.lostNodeId} is lost")
            Right(Some(confirmer))

      case _ =>
        logger.info(s"${request.from} teaches clusterState=${request.clusterState}")
        Right(None)

  private def ifManuallyConfirmed(event: ClusterNodeLostEvent): Option[Confirmer] =
    _nodeToLossRejected.get(event.lostNodeId).flatMap(_.manuallyConfirmed(event))

  // User manually confirms a ClusterNodeLostEvent event
  def manuallyConfirmNodeLoss(lostNodeId: NodeId, confirmer: Confirmer): IO[Checked[Unit]] =
    logger.debugIO("manuallyConfirmNodeLoss", (lostNodeId, confirmer)):
      lock.lock:
        IO:
          matchConfirmationWithRejection(lostNodeId).map: lossRejected =>
            _nodeToLossRejected.clear()
            _nodeToLossRejected(lostNodeId) = lossRejected.copy(
              manualConfirmer = Some(confirmer))

  private def matchConfirmationWithRejection(lostNodeId: NodeId): Checked[LossRejected] =
    (_nodeToLossRejected.get(lostNodeId), _state.ifNormal.map(_.clusterState)) match
      case (Some(lossRejected), None | Some(_: Coupled))
        if lossRejected.event.lostNodeId == lostNodeId =>
        Right(lossRejected)

      case (maybeLossRejected, maybeClusterState) =>
        val problem = ClusterNodeIsNotLostProblem(lostNodeId,
          View(
            maybeLossRejected,
            Some(maybeClusterState.fold("untaught")(_.toShortString))
          ).flatten.mkString(" • "))
        logger.debug(s"⚠️  $problem${maybeLossRejected.fold("")(o => s" • $o")}")
        Left(problem)

  @TestOnly
  def clusterNodeLossEventToBeConfirmed(lostNodeId: NodeId): Option[ClusterNodeLostEvent] =
    // Not guarded by lock
    _nodeToLossRejected.get(lostNodeId)
      //?.filterNot(_.manualConfirmer.isDefined)
      .map(_.event)

  @TestOnly
  private[cluster] def isActive(id: NodeId): Checked[Boolean] =
    clusterState().map(_.activeId == id)

  def clusterState(): Checked[HasNodes] =
    _state.ifNormal.toRight(UntaughtClusterWatchProblem)
      .map(_.clusterState)

  private def stateString: String =
    _state.toString

  override def toString: String =
    "ClusterWatch(" +
      _state.match
        case Untaught => "untaught"
        case state: Normal =>
          state.clusterState.toShortString/* + ", " +
            state.lastHeartbeat.elapsed.pretty + " ago"*/
        case state: AskingNodeLoss =>
          s"AskingNodeLoss(${state.ifNormal.fold("")(_.clusterState.toShortString)} ${
            state.request.event.toShortString}"
      + ")"

end ClusterWatch


object ClusterWatch:
  type OnUndecidableClusterNodeLoss = Option[ClusterNodeLossNotConfirmedProblem] => IO[Unit]

  /** ClusterNodeLossEvent has been rejected, but the user may confirm it later. */
  private case class LossRejected(
    event: ClusterNodeLostEvent,
    manualConfirmer: Option[Confirmer] = None):

    def manuallyConfirmed(event: ClusterNodeLostEvent): Option[Confirmer] =
      manualConfirmer.filter(_ => event == this.event)

    override def toString =
      s"LossRejected(${event.toShortString}, confirmer=${manualConfirmer.getOrElse("none")})"


  private[watch] final case class Confirmed(manualConfirmer: Option[Confirmer] = None)
