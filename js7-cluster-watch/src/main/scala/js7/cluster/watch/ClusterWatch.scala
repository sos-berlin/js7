package js7.cluster.watch

import cats.effect.IO
import cats.syntax.option.*
import com.typesafe.scalalogging.Logger as ScalaLogger
import js7.base.catsutils.CatsEffectExtensions.{left, right}
import js7.base.catsutils.SyncDeadline
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.ScalaUtils.{flattenToString, functionCallToString}
import js7.cluster.watch.ClusterWatch.*
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, HasNodes, PassiveLost}
import js7.data.cluster.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem, ClusterWatchEventMismatchProblem, ClusterWatchInactiveNodeProblem, ClusterWatchNotAskingProblem, UntaughtClusterWatchProblem}
import js7.data.cluster.{ClusterState, ClusterWatchAskNodeLoss, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchCommitNodeLoss, ClusterWatchRequest}
import js7.data.node.NodeId
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable

final class ClusterWatch(
  label: String = "",
  onClusterStateChanged: HasNodes => Unit = _ => (),
  requireManualNodeLossConfirmation: Boolean = false,
  checkActiveIsLost: ClusterState.HasNodes => IO[Checked[Unit]],
  onUndecidableClusterNodeLoss: OnUndecidableClusterNodeLoss = _ => IO.unit):

  private val logger = Logger.withPrefix[this.type](label)

  // Variables are synchronized
  private val lock = AsyncLock()
  private val _nodeToLossRejected = mutable.Map.empty[NodeId, LossRejected]
  private var _state: Option[State] = None

  def processRequest(request: ClusterWatchRequest): IO[Checked[Confirmed]] =
    logger.debugIOWithResult(s"processRequest($request)"):
      lock.lock:
        IO.pure(request.checked).flatMapT: _ =>
          processRequest2(request)

  private def processRequest2(request: ClusterWatchRequest): IO[Checked[Confirmed]] =
    import request.from

    SyncDeadline.now.flatMap: now =>
      lazy val opString = s"${request.maybeEvent.fold("")(o => s"$o --> ")}${request.clusterState}"
      logger.trace(s"$from: $opString${
        _state.fold("")(o => ", after " + o.lastHeartbeat.elapsed(using now).pretty)}")

      for case state: AskingNodeLoss <- _state do
        // Maybe invalidate current AskingNodeLoss
        if now >= state.until then
          _state = state.savedNormal
          logger.debug(s"_state = $stateString, because AskingNodeLoss expired")
        else if !request.isSubtypeOf[ClusterWatchCommitNodeLoss] then
          _state = state.savedNormal
          logger.debug(s"_state = ${_state getOrElse "untaught"
            }, restored because AskingNodeLoss is followed by ${request.toShortString}")

      (_state.flatMap(_.savedNormal), request.maybeEvent, request.forceWhenUntaught) match
        case (None/*untaught*/, Some(event: ClusterNodeLostEvent), /*force=*/false) =>
          IO:
            manuallyConfirmed(event) match
              case None =>
                val problem = ClusterNodeLossNotConfirmedProblem(from, event)
                logger.warn:
                  s"ClusterWatch is untaught, therefore unable to confirm a node loss: $problem"
                Left(problem)

              case Some(confirmer) =>
                if !request.isSubtypeOf[ClusterWatchAskNodeLoss] then
                  logger.info(s"$from teaches clusterState=${request.clusterState
                    } after $confirmer confirmed${event.getClass.simpleScalaName}")
                Right(Some(confirmer) -> request.clusterState)

        case (None, _, _) =>
          if !request.isSubtypeOf[ClusterWatchAskNodeLoss] then
            logger.info(s"$from teaches clusterState=${request.clusterState}")
          IO.right(None -> request.clusterState)

        case (Some(state), _, _) =>
          state.processRequest(request, manuallyConfirmed, checkActiveIsLost, opString)(using now)
    .flatMap:
      case Left(problem: ClusterNodeLossNotConfirmedProblem) =>
        SyncDeadline.usingNow: now ?=>
          _nodeToLossRejected(problem.event.lostNodeId) = LossRejected(problem.event)
          _state.flatMap(_.savedNormal).foreach: state =>
            if state.clusterState.activeId == from then
              _state = Some(state.setLastHeartbeat(now))
              logger.debug(s"_state = $state, $problem")
        .flatMap: _ =>
          onUndecidableClusterNodeLoss(Some(problem))
            .as(Left(problem))

      case Left(problem) =>
        _nodeToLossRejected.clear()
        IO.left(problem)

      case Right((maybeManualConfirmer, updatedClusterState)) =>
        SyncDeadline.now.flatMap: now =>
          request match
            case request: ClusterWatchAskNodeLoss =>
              _state match
                case None =>
                  maybeManualConfirmer match
                    case None => IO.left(UntaughtClusterWatchProblem)
                    case Some(manualConfirmer) =>
                      IO:
                        updatedClusterState.checkedSubtype[ClusterState.IsNodeLost]
                          .map: updatedClusterState =>
                            _state = Some(AskingNodeLoss(
                              logger,
                              clusterState = request.clusterState/*???*/,
                              requireManualNodeLossConfirmation = requireManualNodeLossConfirmation,
                              request = request,
                              askedClusterState = updatedClusterState,
                              lastHeartbeat = now/*???*/,
                              until = now + request.hold,
                              savedNormal = None))
                            logger.debug(s"_state = $stateString")
                            Confirmed(manualConfirmer = Some(manualConfirmer))

                case Some(state) =>
                  IO:
                    updatedClusterState.checkedSubtype[ClusterState.IsNodeLost]
                      .map: updatedClusterState =>
                        _state = Some(AskingNodeLoss(
                          logger,
                          clusterState = request.clusterState/*???*/,
                          requireManualNodeLossConfirmation = requireManualNodeLossConfirmation,
                          request = request,
                          askedClusterState = updatedClusterState,
                          lastHeartbeat = now/*???*/,
                          until = now + request.hold,
                          savedNormal = state match
                            case state: Normal => Some(state)
                            case state: AskingNodeLoss => state.savedNormal))
                        logger.debug(s"_state = $stateString")
                        Confirmed(manualConfirmer = maybeManualConfirmer)

            case request: ClusterWatchCommitNodeLoss =>
              _state match
                case Some(asking: AskingNodeLoss) =>
                  if asking.until <= now then
                    logger.warn(s"${request.toShortString} expires")
                    _state = asking.savedNormal
                    logger.debug(s"_state = $stateString")
                    IO.left(ClusterWatchNotAskingProblem) // Client should ask again
                  else if updatedClusterState != asking.askedClusterState  then
                    logger.warn(s"${request.toShortString}: ${updatedClusterState.toShortString
                      } does not match previously asked ${asking.askedClusterState.toShortString}")
                    IO.left(ClusterWatchNotAskingProblem)
                  else if request.event != asking.request.event then
                    logger.warn(s"${request.toShortString}: ${request.event
                      } does not match previously asked ${asking.request.event}")
                    IO.left(ClusterWatchNotAskingProblem)
                  else
                    _nodeToLossRejected.clear()
                    val changed = !_state.flatMap(_.savedNormal).exists:
                      _.clusterState == updatedClusterState
                    _state = Some(Normal(
                      logger,
                      updatedClusterState,
                      lastHeartbeat = now,
                      requireManualNodeLossConfirmation = requireManualNodeLossConfirmation))
                    if changed then logger.debug(s"_state = $stateString")

                    if changed then
                      onClusterStateChanged(updatedClusterState)

                    maybeManualConfirmer.foldMap: _ =>
                      onUndecidableClusterNodeLoss(None)
                    .as(Right(Confirmed(manualConfirmer = maybeManualConfirmer)))

                case _ =>
                  logger.warn(s"${request.toShortString
                    }: not asking, or active cluster node sent a heartbeat • ${
                    _state getOrElse "untaught"}")
                  IO.left(ClusterWatchNotAskingProblem)

            case request: (ClusterWatchCheckEvent | ClusterWatchCheckState) =>
              _nodeToLossRejected.clear()
              val changed = !_state.flatMap(_.savedNormal).exists:
                _.clusterState == updatedClusterState
              _state = Some(Normal(
                logger,
                updatedClusterState,
                lastHeartbeat = now,
                requireManualNodeLossConfirmation = requireManualNodeLossConfirmation))
              if changed then logger.debug(s"_state = $stateString")

              if changed then
                onClusterStateChanged(updatedClusterState)

              maybeManualConfirmer.foldMap: _ =>
                onUndecidableClusterNodeLoss(None)
              .as(Right(Confirmed(manualConfirmer = maybeManualConfirmer)))
  end processRequest2

  // User manually confirms a ClusterNodeLostEvent event
  def manuallyConfirmNodeLoss(lostNodeId: NodeId, confirmer: String): IO[Checked[Unit]] =
    logger.debugIO("manuallyConfirmNodeLoss", (lostNodeId, confirmer)):
      lock.lock:
        IO:
          matchRejectedNodeLostEvent(lostNodeId).map: lossRejected =>
            _nodeToLossRejected.clear()
            _nodeToLossRejected(lostNodeId) = lossRejected.copy(
              manualConfirmer = Some(confirmer))

  private def manuallyConfirmed(event: ClusterNodeLostEvent): Option[String] =
    _nodeToLossRejected.get(event.lostNodeId).flatMap(_.manuallyConfirmed(event))

  private def matchRejectedNodeLostEvent(lostNodeId: NodeId): Checked[LossRejected] =
    (_nodeToLossRejected.get(lostNodeId), _state.flatMap(_.savedNormal).map(_.clusterState)) match
      case (Some(lossRejected), None)
        if lossRejected.event.lostNodeId == lostNodeId =>
        Right(lossRejected)

      case (Some(lossRejected), Some(Coupled(setting)))
        if lossRejected.event.lostNodeId == lostNodeId && lostNodeId == setting.activeId =>
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
    _state.flatMap(_.savedNormal).toChecked(UntaughtClusterWatchProblem)
      .map(_.clusterState)

  private def stateString =
    _state getOrElse "untaught"

  override def toString: String =
    "ClusterWatch(" +
      _state.fold("untaught"):
        case state: Normal =>
          state.clusterState.toShortString/* + ", " +
            state.lastHeartbeat.elapsed.pretty + " ago"*/
        case state: AskingNodeLoss =>
          s"AskingNodeLoss(${state.savedNormal.fold("")(_.clusterState.toShortString)} ${
            state.request.event.toShortString}"
      + ")"


  /** The ClusterWatch has two states: Normal, and AskingNodeLoss.
    */
  private[ClusterWatch] trait State:
    val logger: ScalaLogger
    val clusterState: HasNodes
    val lastHeartbeat: SyncDeadline
    val requireManualNodeLossConfirmation: Boolean

    def savedNormal: Option[Normal]

    def processRequest(
      request: ClusterWatchRequest,
      manuallyConfirmed: ClusterNodeLostEvent => Option[String],
      checkActiveIsLost: ClusterState.HasNodes => IO[Checked[Unit]],
      opString: => String)
      (using now: SyncDeadline.Now)
    : IO[Checked[(manualConfirmer: Option[String], clusterState: HasNodes)]] =
      import request.{from, maybeEvent, clusterState as reportedClusterState}

      if !request.isSubtypeOf[ClusterWatchAskNodeLoss]
        && !request.isSubtypeOf[ClusterWatchCommitNodeLoss]
        && reportedClusterState == clusterState
      then
        if maybeEvent.nonEmpty then
          logger.debug:
            s"$from: 🪱 Ignore probably duplicate event for already reached clusterState=$clusterState"
        IO.right(None -> reportedClusterState)
      else
        def clusterWatchInactiveNodeProblem =
          ClusterWatchInactiveNodeProblem(from, clusterState, lastHeartbeat.elapsed, opString)
        maybeEvent.match
          case Some(_: ClusterSwitchedOver) =>
            // ClusterSwitchedOver is applied by each node and is considered reliable
            IO.right(())

          case Some(ClusterFailedOver(failedActiveId, _, _)) =>
            clusterState match
              case PassiveLost(setting) if setting.activeId == failedActiveId =>
                IO.left(ClusterFailOverWhilePassiveLostProblem)

              case clusterState =>
                IO.pure:
                  (from == clusterState.passiveId && !isLastHeartbeatStillValid) !!
                    clusterWatchInactiveNodeProblem
                .flatMapT: _ =>
                  checkActiveIsLost(clusterState)
          case _ =>
            IO.pure:
              (from == clusterState.activeId) !! clusterWatchInactiveNodeProblem
        .flatMapT: _ =>
          clusterState.applyEvents(maybeEvent) match
            case Left(problem) =>
              logger.warn(s"$from: $problem")
              IO.left(ClusterWatchEventMismatchProblem(
                maybeEvent, clusterState, reportedClusterState = reportedClusterState))

            case Right(updatedClusterState) =>
              for event <- maybeEvent do logger.info(s"$from $event")
              val confirmer = maybeEvent match
                case Some(event: ClusterNodeLostEvent) => manuallyConfirmed(event)
                case _ => None
              maybeEvent match
                case Some(event: ClusterNodeLostEvent)
                  if requireManualNodeLossConfirmation && !confirmer.isDefined =>
                  IO.left(ClusterNodeLossNotConfirmedProblem(from, event))
                case _ =>
                  if updatedClusterState == reportedClusterState then
                    logger.info(s"$from changes ClusterState to $reportedClusterState")
                    IO.right(confirmer -> reportedClusterState)
                  else
                    // The node may have died just between sending the event to
                    // ClusterWatch and persisting it. Then we have a different state.
                    val previouslyActive = clusterState.activeId.string
                    logger.warn(s"$from forced ClusterState to $reportedClusterState " +
                      s"maybe because the last heartbeat of up to now active $previouslyActive " +
                      s"is too long ago (${lastHeartbeat.elapsed.pretty})")
                    IO.right(confirmer -> reportedClusterState)
        .flatTap:
          case Left(problem) => IO(logger.warn(s"$from: $problem"))
          case Right(_) => IO.unit

    private def isLastHeartbeatStillValid(using SyncDeadline.Now) =
      (lastHeartbeat + clusterState.timing.clusterWatchHeartbeatValidDuration).hasTimeLeft


  private[ClusterWatch] final case class Normal(
    logger: ScalaLogger,
    clusterState: HasNodes,
    lastHeartbeat: SyncDeadline,
    requireManualNodeLossConfirmation: Boolean)
  extends State:

    def savedNormal = Some(this)

    def setLastHeartbeat(now: SyncDeadline.Now): Normal =
      copy(lastHeartbeat = now)

    override def toString =
      functionCallToString("Normal",
        clusterState.toShortString.some,
        s"lastHeartbeat=$lastHeartbeat".some,
        requireManualNodeLossConfirmation ? "requireManualNodeLossConfirmation")


  private[ClusterWatch] final case class AskingNodeLoss(
    logger: ScalaLogger,
    clusterState: ClusterState.HasNodes,
    lastHeartbeat: SyncDeadline,
    requireManualNodeLossConfirmation: Boolean,
    request: ClusterWatchAskNodeLoss,
    askedClusterState: ClusterState.IsNodeLost,
    until: SyncDeadline,
    savedNormal: Option[Normal])
  extends State:

    def matches(commit: ClusterWatchCommitNodeLoss): Checked[Unit] =
      if commit.from == request.from
        && commit.clusterState == request.clusterState
        && commit.event == request.event
        && commit.forceWhenUntaught == request.forceWhenUntaught
      then
        Checked.unit
      else
        Left(ClusterWatchNotAskingProblem)

    override def toString =
      functionCallToString("AskingNodeLoss",
        clusterState.toShortString.some,
        s"lastHeartbeat=$lastHeartbeat".some,
        requireManualNodeLossConfirmation ? "requireManualNodeLossConfirmation",
        request.some,
        s"askedClusterState=${askedClusterState.toShortString}".some,
        s"until=$until".some,
        s"savedNormal=$savedNormal".some)


object ClusterWatch:
  type OnUndecidableClusterNodeLoss = Option[ClusterNodeLossNotConfirmedProblem] => IO[Unit]

  /** ClusterNodeLossEvent has been rejected, but the user may confirm it later. */
  private case class LossRejected(
    event: ClusterNodeLostEvent,
    manualConfirmer: Option[String] = None):

    def manuallyConfirmed(event: ClusterNodeLostEvent): Option[String] =
      manualConfirmer.filter(_ => event == this.event)

    override def toString = s"LossRejected(${event.toShortString}, $manualConfirmer)"


  private[watch] final case class Confirmed(manualConfirmer: Option[String] = None)
