package js7.cluster

import cats.effect.{Deferred, FiberIO, IO}
import cats.syntax.monoid.*
import fs2.Stream
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.CatsExtensions.{tryIt, untry}
import js7.base.catsutils.{FiberVar, SyncDeadline}
import js7.base.configutils.Configs.RichConfig
import js7.base.fs2utils.StreamExtensions.{interruptWhenF, onlyNewest}
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.*
import js7.base.monixutils.StreamPauseDetector.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.system.startup.Halt
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.base.utils.{AsyncLock, Atomic, CatsUtils, SetOnce}
import js7.base.web.{HttpClient, Uri}
import js7.cluster.ActiveClusterNode.*
import js7.cluster.watch.api.ClusterWatchConfirmation
import js7.common.http.RecouplingStreamReader
import js7.data.Problems.{AckFromActiveClusterNodeProblem, ClusterCommandInapplicableProblem, ClusterModuleShuttingDownProblem, ClusterNodeIsNotActiveProblem, ClusterSettingNotUpdatable, MissingPassiveClusterNodeHeartbeatProblem, PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem}
import js7.data.cluster.ClusterCommand.{ClusterConfirmCoupling, ClusterStartBackupNode}
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterPassiveLost, ClusterSettingUpdated, ClusterSwitchedOver, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{ActiveShutDown, Coupled, Empty, HasNodes, IsDecoupled, NodesAppointed, PassiveLost, PreparedToBeCoupled}
import js7.data.cluster.ClusterWatchProblems.{ClusterStateEmptyProblem, NoClusterWatchProblem}
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterState, ClusterTiming, ClusterWatchId}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{ClusterableState, EventId, KeyedEvent, NoKeyEvent, Stamped}
import js7.data.item.BasicItemEvent.ItemAttachedToMe
import js7.data.node.NodeId
import js7.journal.state.FileJournal
import scala.concurrent.duration.*
import scala.util.{Failure, Right, Success, Try}

/** Active Cluster node active which is part of a cluster (ClusterState != Empty). */
final class ActiveClusterNode[S <: ClusterableState[S]] private[cluster](
  journal: FileJournal[S],
  passiveNodeUserAndPassword: Option[UserAndPassword],
  common: ClusterCommon,
  clusterConf: ClusterConf):

  private val keepAlive = clusterConf.config.finiteDuration("js7.web.client.keep-alive").orThrow
  private val clusterStateLock = AsyncLock(logMinor = true)
  private val isFetchingAcks = Atomic(false)
  private val fetchingAcks = new FiberVar[Unit]
  private val fetchingAcksTerminatedUnexpectedlyPromise =
    Deferred.unsafe[IO, Try[Checked[Completed]]]
  private val startingBackupNode = Atomic(false)
  private val sendingClusterStartBackupNode = SetOnce[FiberIO[Unit]]
  @volatile private var stopAcknowledgingRequested = false
  private val stopAcknowledging = Deferred.unsafe[IO, Unit]
  @volatile private var stopRequested = false
  private val clusterWatchSynchronizerOnce = SetOnce[ClusterWatchSynchronizer]

  private def clusterWatchSynchronizer = clusterWatchSynchronizerOnce.orThrow

  import clusterConf.ownId

  def start(eventId: EventId): IO[Checked[Unit]] =
    logger.debugIO:
      // When ClusterWatchId changes at start, an ClusterWatchRegistered event is emitted,
      // changing the ClusterState (the ClusterWatchId part).
      // In this case we continue with the updated ClusterState
      def currentClusterState = journal.clusterState.map(_.asInstanceOf[HasNodes])

      currentClusterState
        .<*(clusterStateLock.lock:
          journal.journaler.onPassiveLost *>
            journal.persist: state =>
              state.clusterState match
                case clusterState: Coupled =>
                  // ClusterPassiveLost because then a ClusterWatchRegistered due to ClusterWatch change
                  // does not require a passive node acknowledge which would followed by a deadlock
                  // because this ActiveClusterNode is not ready yet.
                  // Anyway, the PassiveClusterNode would try to send a ClusterRecouple to provoke
                  // a ClusterPassiveLost. But we do it first.
                  assert(clusterState.isNonEmptyActive(ownId))
                  Right(Seq(
                    NoKey <-: ClusterPassiveLost(clusterState.passiveId)))
                case _ => Right(Nil))
        .flatMap: initialClusterState =>
          // ClusterState may have changed to PassiveLost but here we look at initialClusterState
          assertThat(initialClusterState.activeId == ownId)
          clusterStateLock.lock(IO
            .parMap2(
              // .start requires a locked clusterStateLock!
              IO.defer:
                clusterWatchSynchronizerOnce :=
                  common.initialClusterWatchSynchronizer(initialClusterState)
                // clusterStateLock must be locked:
                clusterWatchSynchronizer.start(currentClusterState, registerClusterWatchId),
              awaitAcknowledgmentIfCoupled(initialClusterState, eventId)
            )(_ |+| _)
            .flatMapT(_ => proceed(initialClusterState).as(Checked.unit)))

  private def awaitAcknowledgmentIfCoupled(initialClusterState: HasNodes, eventId: EventId)
  : IO[Checked[Completed]] =
    initialClusterState match
      case clusterState @ (_: Coupled | _: ActiveShutDown) =>
        logger.info:
          s"↘ Requesting the passive node's acknowledgement for the last recovered event (${
            EventId.toString(eventId)})"
        awaitAcknowledgement(clusterState.passiveUri, eventId)
          .flatMapT: ackEventId =>
            // In case of ClusterWatchRegistered, check journal.currentState.eventId too
            if ackEventId == eventId || ackEventId == journal.unsafeCurrentState().eventId then
              logger.info("↙ Passive node acknowledged the recovered state")
              IO.right(Completed)
            else
              IO.left(Problem:
                s"Passive Cluster node most recently acknowledged ${
                  EventId.toString(ackEventId)} which is NOT the expected EventId")
      case _ => IO.right(Completed)

  def stop: IO[Unit] =
    logger.debugIO(IO.defer:
      stopRequested = true
      IO.both(
        logger.traceIO("fetchingAcks cancel"):
          fetchingAcks.cancel,
        logger.traceIO("clusterWatchSynchronizerOnce stop"):
          clusterWatchSynchronizerOnce.toOption.fold(IO.unit)(_.stop)
      ).void)

  def beforeJournalingStarts: IO[Checked[Unit]] =
    IO.defer:
      logger.trace("beforeJournalingStarts")
      journal.clusterState flatMap:
        case clusterState: Coupled =>
          // Inhibit activation of peer again. If recovery or asking ClusterWatch took a long time,
          // peer may have activated itself.
          common
            .inhibitActivationOfPeer(clusterState, passiveNodeUserAndPassword)
            .map:
              case Some(otherFailedOver) =>
                Left(Problem.pure(
                  s"While activating this node, the other node has failed-over: $otherFailedOver"))
              case None =>
                Right(Completed)
        case _ =>
          IO.right(Completed)

  private[cluster] def changePassiveUri(
    passiveUri: Uri,
    extraEvent: Option[ItemAttachedToMe] = None)
  : IO[Checked[Unit]] =
    logger.debugIO:
      common.requireValidLicense.flatMapT: _ =>
        clusterStateLock.lock:
          suspendHeartbeat(forEvent = true):
            journal.state.map(_.clusterState).flatMap:
              case clusterState: Coupled if passiveUri != clusterState.setting.passiveUri =>
                journal.journaler.onPassiveLost *>
                  persistWithoutTouchingHeartbeat(dontAskClusterWatchWhenUntaught = true /*Since v2.7*/):
                    case clusterState: Coupled =>
                      Right:
                        (passiveUri != clusterState.setting.passiveUri) ?
                          ClusterPassiveLost(clusterState.passiveId) // Forces recoupling
                    case _ => Right(None)
              case _ => IO.right(())
            .flatMapT: _ =>
              persistWithoutTouchingHeartbeat(extraEvent):
                case clusterState: HasNodes =>
                  if passiveUri == clusterState.setting.passiveUri then
                    Right(None)
                  else if clusterState.isInstanceOf[Coupled] then
                    // ClusterPassiveLost above should have avoided this
                    Left(PassiveClusterNodeUrlChangeableOnlyWhenNotCoupledProblem)
                  else
                    Right(Some(ClusterSettingUpdated(Some(passiveUri))))

                case clusterState =>
                  Left(ClusterSettingNotUpdatable(clusterState))
              .flatMapT:
                case (stampedEvents, state: HasNodes) if stampedEvents.nonEmpty =>
                  proceedNodesAppointed(state).as(Right(()))
                case _ =>
                  IO.right(())

  private[cluster] def onRestartActiveNode: IO[Checked[Completed]] =
    clusterStateLock.lock(
      persist():
        case _: ActiveShutDown =>
          Right(Some(ClusterActiveNodeRestarted))
        case _ =>
          Right(None)
      .mapt(_ => Completed))

  def executeCommand(command: ClusterCommand): IO[Checked[ClusterCommand.Response]] =
    command match
      case _: ClusterCommand.ClusterStartBackupNode =>
        throw new AssertionError("ClusterStartBackupNode at the active node?")

      case _: ClusterCommand.ClusterConfirmCoupling =>
        throw new AssertionError("ClusterConfirmCoupling at the active node?")

      case command @ ClusterCommand.ClusterPrepareCoupling(activeId, passiveId, _) =>
        requireOwnNodeId(command, activeId):
          IO.defer:
            if stopAcknowledgingRequested | stopRequested then
              // TODO Cancel also when ack is stopped after execution has been started
              IO.left(Problem.pure("Active cluster node is shutting down"))
            else
              clusterStateLock.lock(command.toShortString):
                checkCouplingToken(command).flatMapT: _ =>
                  persist():
                    case Empty =>
                      Left(ClusterCommandInapplicableProblem(command, Empty))

                    case clusterState: HasNodes =>
                      if clusterState.activeId != activeId || clusterState.passiveId != passiveId then
                        Left(ClusterCommandInapplicableProblem(command, clusterState))
                      else
                        clusterState match
                          case _: IsDecoupled =>
                            Right(Some(ClusterCouplingPrepared(activeId)))

                          case _: PreparedToBeCoupled | _: Coupled | _: ActiveShutDown =>
                            logger.debug:
                              s"ClusterPrepareCoupling command ignored in clusterState=$clusterState"
                            Right(None)
                  .flatMapT: (stampedEvents, clusterState) =>
                    proceed(clusterState).unless(stampedEvents.isEmpty)
                      .as(Right(ClusterCommand.Response.Accepted))

      case command @ ClusterCommand.ClusterCouple(activeId, passiveId, _) =>
        requireOwnNodeId(command, activeId):
          checkCouplingToken(command).flatMapT: _ =>
            clusterStateLock.lock(command.toShortString):
              persist():
                case clusterState @ ClusterState.Empty =>
                  Left(ClusterCommandInapplicableProblem(command, clusterState))

                case clusterState: HasNodes =>
                  if clusterState.activeId != activeId || clusterState.passiveId != passiveId then
                    Left(ClusterCommandInapplicableProblem(command, clusterState))
                  else
                    clusterState match
                      case _: PassiveLost =>
                        // Happens when this active node has restarted just before the passive one
                        // and has already emitted a PassiveLost event.
                        // We ignore this.
                        // The passive node will replicate PassiveLost event and recouple
                        Right(None)

                      case s: PreparedToBeCoupled =>
                        // This is the normally expected ClusterState
                        if !s.setting.clusterWatchId.isDefined then
                        // Passive cluster tries again until
                        // ClusterWatch has been registered via background ClusterWatch heartbeat
                          Left(NoClusterWatchProblem)
                        else
                          Right(Some(ClusterCoupled(activeId)))

                      case _: Coupled =>
                        // Already coupled
                        Right(None)

                      case s =>
                        Left(ClusterCommandInapplicableProblem(command, s))
              .flatMapT: (stampedEvents, state) =>
                proceed(state).unless(stampedEvents.isEmpty)
                  .as(Right(ClusterCommand.Response.Accepted))

      case ClusterCommand.ClusterRecouple(activeId, passiveId) =>
        requireOwnNodeId(command, activeId):
          clusterStateLock.lock(command.toShortString):
            journal.journaler.onPassiveLost *>
              persist():
                case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
                  // ClusterPassiveLost leads to recoupling
                  // TODO The cluster is not coupled for a short while.
                  Right(Some(ClusterPassiveLost(passiveId)))

                case _ =>
                  Right(None)
              .map(_.map(_ => ClusterCommand.Response.Accepted))

      case ClusterCommand.ClusterPassiveDown(activeId, passiveId) =>
        requireOwnNodeId(command, activeId)(IO.defer:
         logger.info(s"The passive $passiveId is shutting down")
          // DEADLOCK when shutdownThisNode wait for ack: clusterStateLock.lock(command.toShortString):
          journal.journaler.onPassiveLost *>
            persist():
              case s: Coupled if s.activeId == activeId && s.passiveId == passiveId =>
                Right(Some(ClusterPassiveLost(passiveId)))

              case _ =>
                Right(None)
            .map(_.map(_ => ClusterCommand.Response.Accepted)))

      case _: ClusterCommand.ClusterInhibitActivation =>
        throw new NotImplementedError

  private def requireOwnNodeId[A](command: ClusterCommand, nodeId: NodeId)(body: IO[Checked[A]])
  : IO[Checked[A]] =
    if nodeId != ownId then
      IO.left(Problem.pure(
        s"'${command.getClass.simpleScalaName}' command may only be directed to the active node"))
    else
      body

  // TODO ClusterCouplingCommand soll zufällige Kennung mitgeben
  //  Neues Kommando vom Aktiven an Passiven, dass den Passiven die Kennung prüft lässt
  //  ClusterCheckCoupling(Base64UUID)
  private def checkCouplingToken(command: ClusterCommand.ClusterCouplingCommand)
  : IO[Checked[Unit]] =
    logger.debugIO:
      journal.state.map(_.clusterState)
        .map:
          case o @ ClusterState.Empty => Left(ClusterCommandInapplicableProblem(command, o): Problem)
          case HasNodes(setting) => Right(setting.passiveUri)
        .flatMapT: passiveUri =>
          common.clusterNodeApi(
            Admission(passiveUri, passiveNodeUserAndPassword),
            "checkCouplingToken")
          .use: api =>
            api.login(onlyIfNotLoggedIn = true) *>
              HttpClient.liftProblem:
                api.executeClusterCommand(ClusterConfirmCoupling(command.token)).void
          .timeoutTo(passiveNodeCouplingResponseTimeout, IO.left(Problem(
            s"Passive node did not respond within ${passiveNodeCouplingResponseTimeout.pretty}")))
          .handleError: throwable =>
            val msg = s"A passive cluster node wanted to couple but $passiveUri does not respond"
            logger.error(s"$msg: ${throwable.toStringWithCauses}")
            Left(Problem(msg))

  def switchOver: IO[Checked[Completed]] =
    clusterStateLock.lock:
      persist():
        case coupled: Coupled =>
          Right(Some(ClusterSwitchedOver(coupled.passiveId)))
        case state =>
          Left(Problem.pure("Switchover is possible only for the active and coupled cluster node," +
            s" but cluster state is: $state"))
      .flatMapT: (_: Seq[Stamped[?]], _) =>
        stopAcknowledgingRequested = true
        stopAcknowledging.complete(())
          .as(Right(Completed))

  def shutDownThisNode: IO[Checked[Completed]] =
    logger.traceIOWithResult:
      // FIXME Deadlock when events are not yet acknowledged while this node shuts down
      // Journal kann blockieren, wenn ein Commit noch nicht bestätigt worden ist.
      // Lösungsvorschlag: Journal ordentlich beenden
      //  - ClusterActiveNodeShutDown
      //  - Ausstehende Acks abwarten
      //  - Derweil kann ClusterPassiveLost auftreten.
      //  - Erst nach Erfolg oder ClusterPassiveLost die Acks abschalten.
      //  --> Das alles in eine stopJournaling-Routine? --> ResourceIO["Journaling"]
      clusterStateLock.lock:
        persist():
          case _: Coupled =>
            Right(Some(ClusterActiveNodeShutDown))
          case _ =>
            Right(None)
        .recoverT:
          case ClusterModuleShuttingDownProblem => ()
        .flatMapT: _ =>
          stopAcknowledgingRequested = true
          stopAcknowledging.complete(())
            .as(Right(Completed))

  private def proceed(state: ClusterState): IO[Completed] =
    state match
      case state: NodesAppointed =>
        proceedNodesAppointed(state)

      case state: Coupled =>
        proceedCoupled(state)

      case _ => IO.completed

  private def proceedNodesAppointed(clusterState: HasNodes): IO[Completed] =
    logger.traceIO:
      clusterState match
        case clusterState: NodesAppointed =>
          IO.unlessA(startingBackupNode.getAndSet(true)):
            startSendingClusterStartBackupNode(clusterState)
          .as(Completed)

        case _ =>
          IO.completed

  private def startSendingClusterStartBackupNode(clusterState: NodesAppointed): IO[Unit] =
    journal.eventWatch.started
      .*>(journal.state)
      .flatMap(state => common
        .tryEndlesslyToSendCommand(
          Admission(clusterState.passiveUri, passiveNodeUserAndPassword),
          ClusterStartBackupNode(
            clusterState.setting,
            fileEventId = journal.eventWatch.lastFileEventId,
            activeNodeName =
              state.clusterNodeIdToName(clusterState.activeId).orThrow,
            passiveNodeUserId =
              state.clusterNodeToUserId(clusterState.passiveId).orThrow)))
      .handleError: t =>
        // Bad
        logger.error(s"Sending ClusterStartBackupNode command to backup node failed: $t", t)
      .start
      .flatMap: fiber =>
        IO:
          sendingClusterStartBackupNode := fiber
          ()

  private def proceedCoupled(state: Coupled): IO[Completed] =
    startFetchAndHandleAcknowledgedEventIds(state)

  private def startFetchAndHandleAcknowledgedEventIds(initialState: Coupled): IO[Completed] =
    IO.defer:
      if isFetchingAcks.getAndSet(true) then
        logger.debug("fetchAndHandleAcknowledgedEventIds: already isFetchingAcks")
        IO.pure(Completed)
      else
        import initialState.{passiveId, passiveUri, timing}
        CorrelId
          .bindNew:
            fetchAndHandleAcknowledgedEventIds(passiveId, passiveUri, timing)
          .tryIt
          .flatTap:
            case Success(Left(_: MissingPassiveClusterNodeHeartbeatProblem)) =>
              IO.unit
            case tried =>
              IO.unlessA(stopAcknowledgingRequested | stopRequested):
                // Completes only when not cancelled, and then it is a failure
                fetchingAcksTerminatedUnexpectedlyPromise.complete(tried).void
          .void
          .start
          .flatMap(fetchingAcks.set)
          .as(Completed)

  private def fetchAndHandleAcknowledgedEventIds(
    passiveId: NodeId,
    passiveUri: Uri,
    timing: ClusterTiming)
  : IO[Checked[Completed]] =
    fetchAndHandleAcknowledgedEventIds2(passiveId, passiveUri, timing)
      .flatMap:
        case Left(missingHeartbeatProblem @ MissingPassiveClusterNodeHeartbeatProblem(passiveId, duration)) =>
          logger.warn(s"❗ No heartbeat from passive cluster $passiveId since ${duration.pretty}" +
            " - trying to continue as single active cluster node")
          assertThat(passiveId != ownId)

          // FIXME (1) Exklusiver Zugriff (Lock) wegen parallelen ClusterCommand.ClusterRecouple,
          //  das ein ClusterPassiveLost auslöst, mit ClusterCouplingPrepared infolge.
          //  Dann können wir kein ClusterPassiveLost ausgeben.
          //  Journal Lock in die Anwendungsebene (hier) heben
          //  -- Nicht nötig durch die Abfrage auf initialState ?
          // FIXME (2) Deadlock when called immediately after start of Controller, before Journal has been started ?
          //  journal.awaitCurrentState may not respond to GetJournalState.
          /*
            ClusterPassiveLost kann während eines persist passieren, das auf Ack des Passiven
            wartet.
            Während eines ClusterCoupled ?
            Kein lock hier, wegen möglichen Deadlocks !!!
           */
          journal.forPossibleFailoverByOtherNode:
            journal.clusterState.flatMap:
              case clusterState: Coupled =>
                val passiveLost = ClusterPassiveLost(passiveId)
                suspendHeartbeat(forEvent = true):
                  common.ifClusterWatchAllowsActivation(clusterState, passiveLost):
                    journal.journaler.onPassiveLost *>
                      persistWithoutTouchingHeartbeat():
                        case _: Coupled => Right(Some(passiveLost))
                        case _ => Right(None)  // Ignore when ClusterState has changed (no longer Coupled)
                      .map(_.toCompleted)
                      .rightAs(true)
                .map(_.flatMap: allowed =>
                  if !allowed then
                    Right(Completed)
                  else
                    Left(missingHeartbeatProblem))
              case _ =>
                IO.pure(Left(missingHeartbeatProblem))

        case o =>
          IO.pure(o)
      .materialize.flatTap(tried => IO { tried match
        case Success(Right(Completed)) =>
          if !stopAcknowledgingRequested && !stopRequested then
            logger.error("fetchAndHandleAcknowledgedEventIds terminated unexpectedly")

        case Success(Left(_: MissingPassiveClusterNodeHeartbeatProblem)) =>
          logger.warn("❗ Continue as single active cluster node, without passive node")

        case Success(Left(problem @ ClusterModuleShuttingDownProblem)) =>
          logger.debug(s"fetchAndHandleAcknowledgedEventIds($passiveUri) failed with $problem")

        case Success(Left(problem)) =>
          logger.error(s"fetchAndHandleAcknowledgedEventIds($passiveUri) failed with $problem")

        case Failure(t: ProblemException) if t.problem is AckFromActiveClusterNodeProblem =>
          if isTest then
            throw new RuntimeException(s"🟥 Halt suppressed for testing: ${t.problem}")
          else
            Halt.haltJava("🟥 HALT because other cluster node has become active",
              restart = true)

        case Failure(t) =>
          logger.error(
            s"fetchAndHandleAcknowledgedEventIds($passiveUri) failed with ${t.toStringWithCauses}",
            t)
      })
      .dematerialize
      .guarantee(IO:
        logger.debug("isFetchingAcks := false")
        isFetchingAcks := false)

  private def fetchAndHandleAcknowledgedEventIds2(passiveId: NodeId, passiveUri: Uri, timing: ClusterTiming)
  : IO[Checked[Completed]] =
    logger.debugIOWithResult:
      HttpClient.liftProblem:
        Stream.resource:
          common.clusterNodeApi(
            Admission(passiveUri, passiveNodeUserAndPassword),
            "acknowledgements")
        .flatMap: api =>
          streamEventIds(api,
            heartbeat = timing.heartbeat min keepAlive,
            returnHeartbeatAs = Some(EventId.Heartbeat))
            .through: stream =>
              clusterConf.testAckLossPropertyKey.fold(stream): k => // Testing only
                var logged = false
                stream.filter: _ =>
                  val suppress = sys.props(k).toBoolean
                  if suppress then
                    logger.warn:
                      s"❌❌ Received acknowledgements are suppressed by js7.journal.cluster.TEST-ACK-LOSS=$k"
                    logged = true
                  !suppress
            .detectPauses(timing.passiveLostTimeout)
            .filter(_ != RightEventIdHeartbeat)
            .onlyNewest
            .interruptWhenF:
              // Race condition: may be set too late?
              stopAcknowledging.get *> IO:
                logger.debug("Stop fetchAndHandleAcknowledgedEventIds2 due to stopAcknowledging")
            .flatMap: // Turn into Stream[,Problem]
              case Left(noHeartbeatSince) =>
                Stream.eval:
                  SyncDeadline.usingNow:
                    MissingPassiveClusterNodeHeartbeatProblem(passiveId, noHeartbeatSince.elapsed)
                .flatMap: problem =>
                  logger.debug(s"💥 $problem")
                  Stream.emit(problem)

              case Right(eventId) =>
                Stream.exec:
                  journal.journaler.onPassiveNodeHasAcknowledged(eventId = eventId)
        .head.compile.last // The first problem, if any
        .map(_.toLeft(Completed))
      .map(_.flatten)

  private def awaitAcknowledgement(passiveUri: Uri, eventId: EventId): IO[Checked[EventId]] =
    logger.debugIOWithResult:
      common.clusterNodeApi(
          Admission(passiveUri, passiveNodeUserAndPassword),
          "awaitAcknowledgement")
        .use: api =>
          HttpClient.liftProblem:
            streamEventIds(api, heartbeat = keepAlive)
              .dropWhile(_ < eventId)
              .head
              .compile
              .last
              .map(_.toRight:
                Problem.pure(s"awaitAcknowledgement($eventId): Stream ended unexpectedly"))
          .map(_.flatten)
        .logWhenItTakesLonger("passive cluster node acknowledgement")

  private def streamEventIds(
    api: ClusterNodeApi,
    heartbeat: FiniteDuration,
    returnHeartbeatAs: Option[EventId] = None)
  : Stream[IO, EventId] =
    RecouplingStreamReader
      .stream[EventId, EventId, ClusterNodeApi](
        toIndex = Some(_),
        api,
        clusterConf.recouplingStreamReader,
        after = -1L/*unused, web service returns always the newest EventIds*/,
        getStream = (_: EventId) =>
          HttpClient.liftProblem:
            api.eventIdStream(
                heartbeat = Some(heartbeat),
                returnHeartbeatAs = returnHeartbeatAs)
              .map(_.evalMapChunk:
                case Left(problem) => IO.raiseError(problem.throwable)
                case Right(eventId) => IO.pure(eventId)),
        stopRequested = () => stopRequested)

  def executeClusterWatchConfirm(cmd: ClusterWatchConfirm): IO[Checked[Unit]] =
    common.clusterWatchCounterpart.executeClusterWatchConfirm(cmd)

  // Called back by clusterWatchCounterpart.executeClusterWatchConfirm
  private def registerClusterWatchId(confirmation: ClusterWatchConfirmation, alreadyLocked: Boolean)
  : IO[Checked[Unit]] =
    logger.traceIOWithResult("registerClusterWatchId", confirmation):
      IO.defer:
        if alreadyLocked then
          nonLockingRegisterClusterWatchId(confirmation)
        else
          clusterStateLock.lock:
            nonLockingRegisterClusterWatchId(confirmation)

  private def nonLockingRegisterClusterWatchId(confirmation: ClusterWatchConfirmation)
  : IO[Checked[Unit]] =
    journal.clusterState
      .flatMap: clusterState =>
        IO(isClusterWatchRegistered(clusterState, confirmation.clusterWatchId))
          .flatMapT:
            if _ then // Shortcut
              IO.right(Nil -> clusterState)
            else
              journal.persistTransaction[ClusterEvent](NoKey): s =>
                isClusterWatchRegistered(s.clusterState, confirmation.clusterWatchId)
                  .map(!_ thenList ClusterWatchRegistered(confirmation.clusterWatchId))
              .map(_.map: (stampedEvents, journaledState) =>
                stampedEvents -> journaledState.clusterState)
      .flatTapT: _ =>
        common.clusterWatchCounterpart.onClusterWatchRegistered(confirmation.clusterWatchId)
        .as(Checked.unit)
      .flatMapT: (stampedEvents, clusterState) =>
        val events = stampedEvents.map(_.value.event)
        (events, clusterState) match
          case (Seq(event), clusterState: HasNodes) =>
            clusterWatchSynchronizer
              .applyEvent(
                event, clusterState,
                clusterWatchIdChangeAllowed = event.isInstanceOf[ClusterWatchRegistered])
              .rightAs(())

          case (Seq(), _) => IO.right(())
          case o => throw new MatchError(o)

  // Returns Right(true) iff clusterState settings contains clusterWatchId
  private def isClusterWatchRegistered(clusterState: ClusterState, clusterWatchId: ClusterWatchId)
  : Checked[Boolean] =
    clusterState match
      case ClusterState.Empty => Left(ClusterStateEmptyProblem)
      case hasNodes: HasNodes =>
        if hasNodes.activeId != ownId then
          Left(ClusterNodeIsNotActiveProblem)
        else
          Right(hasNodes.setting.clusterWatchId contains clusterWatchId)

  def onTerminatedUnexpectedly: IO[Checked[Completed]] =
    fetchingAcksTerminatedUnexpectedlyPromise.get.untry

  private def persist()(toEvents: ClusterState => Checked[Option[ClusterEvent]])
  : IO[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    suspendHeartbeat(forEvent = true):
      persistWithoutTouchingHeartbeat()(toEvents)

  private def persistWithoutTouchingHeartbeat(
    extraEvent: Option[ItemAttachedToMe] = None,
    dontAskClusterWatchWhenUntaught: Boolean = false)(
    toEvents: ClusterState => Checked[Option[ClusterEvent]])
  : IO[Checked[(Seq[Stamped[KeyedEvent[ClusterEvent]]], ClusterState)]] =
    IO.defer:
      assertThat(!clusterWatchSynchronizer.isHeartbeating)
      journal
        .persistTransaction[NoKeyEvent](NoKey): state =>
          toEvents(state.clusterState).flatMap:
            case Some(event) if !state.clusterState.isEmptyOrActive(ownId) =>
              Left(Problem(s"ClusterEvent is only allowed on active cluster node: $event"))
            case maybeEvent =>
              Right(extraEvent.toList ::: maybeEvent.toList)
        .flatMapT: (stampedEvents, state) =>
          IO
            .defer:
              assertThat(!clusterWatchSynchronizer.isHeartbeating)
              state.clusterState match
                case Empty | _: NodesAppointed => IO.unit
                case _: HasNodes => sendingClusterStartBackupNode.fold(IO.unit)(_.cancel)
            .flatMap: _ =>
              val clusterStampedEvents = stampedEvents.collect:
                case o @ Stamped(_, _, KeyedEvent(_, _: ClusterEvent)) =>
                  o.asInstanceOf[Stamped[KeyedEvent[ClusterEvent]]]
              val events = clusterStampedEvents.map(_.value.event)
              (events, state.clusterState) match
                case (Seq(event), clusterState: HasNodes) =>
                  clusterWatchSynchronizer
                    .applyEvent(
                      event, clusterState,
                      clusterWatchIdChangeAllowed = true/*
                        events == Seq(ClusterCouplingPrepared(ownId) ||
                        events == Seq(ClusterPassiveLost(clusterState.passiveId))*/,
                      forceWhenUntaught = dontAskClusterWatchWhenUntaught)
                    .flatMapT:
                      case Some(confirmation)
                        // After SwitchOver this ClusterNode is no longer active
                        if clusterState.isNonEmptyActive(ownId) =>
                        // JS-2092 We depend on the ClusterWatch,
                        // that it confirms the event only if it is taught about the ClusterState.
                        // Then, a ClusterWatch change is allowed.
                        registerClusterWatchId(confirmation, alreadyLocked = true)
                      case _ => IO.right(())
                    .rightAs(clusterStampedEvents -> clusterState)

                case (Seq(), clusterState) =>
                  IO.right(clusterStampedEvents -> clusterState)

                case _ =>
                  IO.left(Problem.pure("persistWithoutTouchingHeartbeat does not match"))

  private def suspendHeartbeat[A](forEvent: Boolean = false)(io: IO[Checked[A]])
    (implicit enclosing: sourcecode.Enclosing)
  : IO[Checked[A]] =
    clusterWatchSynchronizer.suspendHeartbeat(journal.clusterState, forEvent = forEvent):
      io


object ActiveClusterNode:
  private val logger = Logger[this.type]
  private val passiveNodeCouplingResponseTimeout = 3.s
  private val RightEventIdHeartbeat = Right(EventId.Heartbeat)
