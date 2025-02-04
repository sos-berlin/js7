package js7.cluster

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.option.*
import fs2.Stream
import io.circe.syntax.*
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{exists, move, size}
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.StandardOpenOption.{APPEND, CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Path, Paths}
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.catsutils.CatsEffectExtensions.{left, right, startAndForget}
import js7.base.catsutils.SyncDeadline
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.StreamExtensions.{interruptWhenF, mapParallelBatch}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.RefCountedResource
import js7.base.monixutils.StreamPauseDetector.detectPauses
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.*
import js7.base.utils.{OneTimeToken, SetOnce}
import js7.base.web.HttpClient
import js7.cluster.ClusterCommon.clusterEventAndStateToString
import js7.cluster.PassiveClusterNode.*
import js7.common.http.RecouplingStreamReader
import js7.common.jsonseq.PositionAnd
import js7.data.Problems.PassiveClusterNodeResetProblem
import js7.data.cluster.ClusterCommand.{ClusterCouple, ClusterPassiveDown, ClusterPrepareCoupling, ClusterRecouple}
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterResetStarted, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, IsDecoupled, PreparedToBeCoupled}
import js7.data.cluster.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, NoClusterWatchProblem, UntaughtClusterWatchProblem}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterSetting, ClusterState}
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken, StampedHeartbeatByteArray}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{ClusterableState, EventId, JournalEvent, JournalId, JournalPosition, JournalSeparators, KeyedEvent, SnapshotableState, SnapshotableStateBuilder, Stamped}
import js7.data.node.{NodeId, NodeName, NodeNameToPassword}
import js7.journal.EventIdGenerator
import js7.journal.files.JournalFiles.extensions.*
import js7.journal.recover.{FileSnapshotableStateBuilder, JournalProgress, Recovered, RecoveredJournalFile}

private[cluster] final class PassiveClusterNode[S <: ClusterableState[S]](
  ownId: NodeId,
  setting: ClusterSetting,
  recovered: Recovered[S]/*TODO The maybe big ClusterableState at start sticks here*/,
  activeNodeName: NodeName,
  passiveUserId: UserId,
  eventIdGenerator: EventIdGenerator,
  /** For backup initialization, only when ClusterState.Empty. */
  initialFileEventId: Option[EventId],
  otherFailed: Boolean,
  clusterConf: ClusterConf,
  common: ClusterCommon)
  (implicit
    S: ClusterableState.Companion[S],
    nodeNameToPassword: NodeNameToPassword[S],
    ioRuntime: IORuntime):

  import clusterConf.journalConf
  import recovered.{eventWatch, journalLocation}
  import setting.{activeId, idToUri}

  private val shutdown = Deferred.unsafe[IO, Unit]

  assertThat(activeId != ownId && setting.passiveId == ownId)
  assertThat(initialFileEventId.isDefined == (recovered.clusterState == ClusterState.Empty))

  private val activeApiCache = new RefCountedResource(common.clusterNodeApi(
    Admission(
      idToUri(activeId),
      nodeNameToPassword(activeNodeName)
        .orThrow
        .map(UserAndPassword(passiveUserId, _))),
    "Active node"))

  private def activeApiResource(implicit src: sourcecode.Enclosing) =
    activeApiCache.resource

  private val stateBuilderAndAccessor = new StateBuilderAndAccessor(recovered.state)
  private var dontActivateBecauseOtherFailedOver = otherFailed
  @volatile private var awaitingCoupledEvent = false
  @volatile private var stopped = false

  def onShutdown(dontNotifyActiveNode: Boolean = false): IO[Unit] =
    logger.debugIO:
      shutdown.complete(()).attempt *>
        IO.unlessA(dontNotifyActiveNode):
          notifyActiveNodeAboutShutdown

  /** Allow the active node to emit ClusterPassiveLost quickly. */
  private def notifyActiveNodeAboutShutdown: IO[Unit] =
    logger.debugIO:
      // Active and passive node may be shut down at the same time. We try to handle this here.
      val untilDecoupled = logger.traceIO:
        ().tailRecM: _ =>
          stateBuilderAndAccessor.state
            .map(_.clusterState)
            .flatMap:
              case _: Coupled => IO.left(()).delayBy(1.s)
              case clusterState => IO.right(clusterState)

      val notifyActive = activeApiResource
        .use(api => api
          .login(onlyIfNotLoggedIn = true)
          .productR:
            api.retryIfSessionLost:
              api.executeClusterCommand(ClusterPassiveDown(activeId = activeId, passiveId = ownId))
          .void
          .handleError(throwable => logger.debug(
            s"ClusterCommand.ClusterPassiveDown failed: ${throwable.toStringWithCauses}",
            throwable.nullIfNoStackTrace)))
        .timeoutTo(clusterConf.timing.heartbeat /*some short time*/ , IO.unit)
        .logWhenItTakesLonger

      IO.race(untilDecoupled, notifyActive.delayBy(50.ms))
        .flatTap:
          case Left(clusterState) =>
            // The connection of the killed notifyActive HTTP request may be still blocked !!!
            // until it is responded (see PekkoHttpClient)
            // It should not disturb the shutdown.
            IO(logger.debug(s"notifyActiveNodeAboutShutdown: clusterState=$clusterState"))
          case Right(()) =>
            IO(logger.debug(
              "notifyActiveNodeAboutShutdown: Active node has been notified about shutdown"))
        .as(())

  def state: IO[S] =
    stateBuilderAndAccessor.state

  /**
    * Runs the passive node until activated or terminated.
    * Returns also a `IO` with the current ClusterState while being passive or active.
    */
  def run(recoveredState: S): IO[Checked[Recovered[S]]] =
    CorrelId.bindNew(logger.debugIO:
      common.requireValidLicense
        .flatMapT(_ => IO.defer:
          val recoveredClusterState = recoveredState.clusterState
          logger.debug(s"recoveredClusterState=$recoveredClusterState")
          assertThat(!stopped)  // Single-use only

          // Delete obsolete journal files left by last run
          if journalConf.deleteObsoleteFiles then
            for f <- recovered.recoveredJournalFile do
              val eventId = f.fileEventId/*release files before the recovered file*/
              eventWatch.releaseEvents(
                recoveredState.journalState.toReleaseEventId(eventId, journalConf.releaseEventsUserIds))

          for o <- recovered.recoveredJournalFile do
            cutJournalFile(o.file, o.length, o.eventId)

          // Other node failed-over while this node was active but lost?
          // Then FailedOver event will be replicated.
          IO.unlessA(otherFailed):
            backgroundNotifyActiveNodeAboutRestart(recoveredClusterState)
          *>
            replicateJournalFiles(recoveredClusterState)
              .guarantee(IO:
                stopped = true)
              .guarantee(activeApiCache.clear)
              .flatTap:
                case Left(PassiveClusterNodeResetProblem) => IO(
                  journalLocation.deleteJournal(ignoreFailure = true))
                case _ => IO.unit
        ))

  private def backgroundNotifyActiveNodeAboutRestart(recoveredClusterState: ClusterState): IO[Unit] =
    recoveredClusterState
      .match
        case ClusterState.Empty =>
          IO.unit

        case _: IsDecoupled =>
          tryEndlesslyToSendCommandInBackground(
            ClusterPrepareCoupling(activeId = activeId, passiveId = ownId, _))

        case _: PreparedToBeCoupled =>
          tryEndlesslyToSendCommandInBackground(
            ClusterCouple(activeId = activeId, passiveId = ownId, _))

        case _: Coupled =>
          // After a quick restart of this passive node, the active node may not yet have noticed the loss.
          // So we send a ClusterRecouple command to force a ClusterPassiveLost event.
          // Then the active node couples again with this passive node,
          // and we are sure to be coupled and up-to-date and may properly fail-over in case of active node loss.
          // The active node ignores this command if it has emitted a ClusterPassiveLost event.
          IO.defer:
            awaitingCoupledEvent = true
            tryEndlesslyToSendCommandInBackground(
              _ => ClusterRecouple(activeId = activeId, passiveId = ownId))

  def confirmCoupling(token: OneTimeToken): Checked[Unit] =
    common.couplingTokenProvider.confirms(token) !!
      Problem("Another passive cluster node wanted to couple")

  private def cutJournalFile(file: Path, length: Long, eventId: EventId): Unit =
    if exists(file) then
      val garbage = size(file) - length
      if garbage > 0 then
        // Partial event or partial transaction
        logger.info(s"Cutting incomplete data ($garbage bytes) at end of ${file.getFileName} at position $length, EventId $eventId ")
        autoClosing(FileChannel.open(file, WRITE)): f =>
          f.truncate(length)

  private def tryEndlesslyToSendClusterPrepareCoupling: IO[Unit] =
    // TODO Delay until we have replicated nearly all events, to avoid a long PreparedCoupled state
    //  Annähernd gleichlaufende Uhren vorausgesetzt, können wir den Zeitstempel des letzten Events heranziehen.
    //  Wenn kein Event kommt? Herzschlag mit Zeitstempel (nicht EventId) versehen (wäre sowieso nützlich)
    //  ["HEARTBEAT", { timestamp: 1234.567 }]
    //  {TYPE: "Heartbeat", eventId: 1234567000, timestamp: 1234.567 }    Herzschlag-Event?
    //  Funktioniert nicht, wenn die Uhren verschieden gehen. Differenz feststellen?
      tryEndlesslyToSendCommand(
        ClusterPrepareCoupling(activeId = activeId, passiveId = ownId, _))

  private def tryEndlesslyToSendCommandInBackground(toCommand: OneTimeToken => ClusterCommand)
  : IO[Unit] =
    tryEndlesslyToSendCommand(toCommand)
      .attempt
      .map:
        case Left(throwable) => logger.error(
          "While notifying the active cluster node about restart of this passive node:" +
            s" ${throwable.toStringWithCauses}", throwable.nullIfNoStackTrace)
        case Right(()) =>
          logger.debug(
            "Active cluster node has been notified about restart of this passive node")
      .startAndForget

  private def tryEndlesslyToSendCommand(toCommand: OneTimeToken => ClusterCommand)
  : IO[Unit] =
    IO
      .race(
        shutdown.get,
        common.tryEndlesslyToSendCommand(activeApiResource, toCommand))
      .flatMap:
        case Left(()) => IO(logger.debug(
          s"◼️  tryEndlesslyToSendClusterCommand(${toCommand.getClass.simpleScalaName}) canceled due to shutdown"))
        case Right(()) => IO.unit

  private def sendClusterCouple: IO[Unit] =
    tryEndlesslyToSendCommand(
      ClusterCouple(activeId = activeId, passiveId = ownId, _))

  private def replicateJournalFiles(recoveredClusterState: ClusterState)
  : IO[Checked[Recovered[S]]] =
    activeApiResource
      .use: activeNodeApi =>
        val start: Continuation.Replicatable = recovered.recoveredJournalFile match
          case None =>
            assertThat(recoveredClusterState == ClusterState.Empty)
            NoLocalJournal(initialFileEventId.get)
          case Some(recoveredJournalFile) =>
            FirstPartialFile(recoveredJournalFile /*, recoveredClusterState*/)
        start.tailRecM: continuation =>
          replicateJournalFile(continuation, () => stateBuilderAndAccessor.newStateBuilder(), activeNodeApi)
           // TODO Herzschlag auch beim Wechsel zur nächsten Journaldatei prüfen
            .map:
              case Left(problem) =>
                Right(Left(problem))

              case Right(continuation) if shouldActivate(continuation.clusterState) =>
                logger.info(s"Activating because ClusterState has become ${continuation.clusterState}")
                // Replace Recovered (forget the old one, do not close, because JournalEventWatch is the same)
                Right(Right(
                  recovered.changeRecoveredJournalFile(continuation.maybeRecoveredJournalFile)))

              case Right(continuation) =>
                Left(continuation)

  private def replicateJournalFile(
    continuation: Continuation.Replicatable,
    newStateBuilder: () => SnapshotableStateBuilder[S],
    activeNodeApi: ClusterNodeApi)
    (implicit ioRuntime: IORuntime)
  : IO[Checked[Continuation.Replicatable]] =
    logger.debugIO("replicateJournalFile", continuation.fileEventId):
      SyncDeadline.now.flatMap: startedAt =>
        replicateJournalFile2(continuation, newStateBuilder, activeNodeApi, startedAt)

  private def replicateJournalFile2(
    continuation: Continuation.Replicatable,
    newStateBuilder: () => SnapshotableStateBuilder[S],
    activeNodeApi: ClusterNodeApi,
    startedAt: SyncDeadline)
    (implicit ioRuntime: IORuntime)
  : IO[Checked[Continuation.Replicatable]] =
    IO.defer:
      import continuation.file

      val builder = new FileSnapshotableStateBuilder(journalFileForInfo = file.getFileName,
        continuation.maybeJournalId, newStateBuilder)

      def releaseEvents(): Unit =
        if journalConf.deleteObsoleteFiles then
          eventWatch.releaseEvents:
            builder.journalState
              .toReleaseEventId(eventWatch.lastFileEventId, journalConf.releaseEventsUserIds)

      val maybeTmpFile = continuation match
        case _: NoLocalJournal | _: NextFile =>
          val tmp = Paths.get(file.toString + TmpSuffix)
          logger.debug(s"Replicating snapshot into temporary journal file ${tmp.getFileName}")
          Some(tmp)

        case _: FirstPartialFile =>
          None

      var out = maybeTmpFile match
        case None => FileChannel.open(file, APPEND)
        case Some(tmp) => FileChannel.open(tmp, CREATE, WRITE, TRUNCATE_EXISTING)

      locally:
        val f = maybeTmpFile getOrElse file
        logger.debug(s"replicateJournalFile size(${f.getFileName})=${size(f)} ${builder.clusterState}")
        assertThat(continuation.fileLength == size(f))

      var isReplicatingHeadOfFile = maybeTmpFile.isDefined
      val replicatedFirstEventPosition = SetOnce.fromOption(continuation.firstEventPosition, "replicatedFirstEventPosition")
      var replicatedFileLength = continuation.fileLength
      var lastProperEventPosition = continuation.lastProperEventPosition
      var _eof = false

      continuation match
        case FirstPartialFile(recoveredJournalFile) =>
          logger.info(s"Start replicating '${file.getFileName}' file after " +
            s"${EventId.toString(recoveredJournalFile.eventId)}, position ${recoveredJournalFile.length}")
          builder.startWithState(JournalProgress.InCommittedEventsSection, Some(recoveredJournalFile.journalHeader),
            eventId = recoveredJournalFile.eventId,
            totalEventCount = recoveredJournalFile.nextJournalHeader.totalEventCount,
            recoveredJournalFile.state)
          eventWatch.onJournalingStarted(file,
            recoveredJournalFile.journalId,
            firstEventPositionAndFileEventId =
              PositionAnd(recoveredJournalFile.firstEventPosition, continuation.fileEventId),
            flushedLengthAndEventId =
              PositionAnd(recoveredJournalFile.length, recoveredJournalFile.eventId),
            isActiveNode = false)

        case _ =>

      val recouplingStreamReader =
        new RecouplingStreamReader[Long/*file position*/, PositionAnd[ByteArray], ClusterNodeApi](
          toIndex = _.position.some,
          clusterConf.recouplingStreamReader):

          def getStream(api: ClusterNodeApi, position: Long) =
            HttpClient.liftProblem(
              api
                .journalStream(
                  JournalPosition(continuation.fileEventId, position),
                  heartbeat = Some(setting.timing.heartbeat),
                  returnHeartbeatAs = Some(StampedHeartbeatByteArray),
                  markEOF = true)
                .map(_
                  .scan(PositionAnd(position, ByteArray.empty/*unused*/)): (s, line) =>
                    PositionAnd(
                      s.position + (if line == StampedHeartbeatByteArray then 0 else line.length),
                      line)
                  .drop(1)))

          def stopRequested = stopped

          override def eof(index: Long) =
            _eof && index >= replicatedFileLength

      // TODO Eine Zeile davor lesen und sicherstellen, dass sie gleich unserer letzten Zeile ist
      recouplingStreamReader.stream(activeNodeApi, after = continuation.fileLength)
        .interruptWhenF(shutdown.get)
        // TODO Aktiver kann JournalFileIsNotReady melden, sendet keinen Herzschlag, ist aber irgendwie am Leben.
        //  observe könnte selbst Ausfall des Aktiven anzeigen, gdw er nicht erreichbar ist
        //  (zB Login klappt nicht, isTemporaryUnreachable).
        //  observe überwacht selbst die Herzschläge, und verbindet sich bei Ausfall erneut.
        //  Dann entfällt hier die Herzschlagüberwachung.
        .handleErrorWith: t =>
          logger.debug(s"observeJournalFile($activeNodeApi, fileEventId=${continuation.fileEventId}, " +
            s"position=${continuation.fileLength}) failed with ${t.toStringWithCauses}", t)
          Stream.raiseError[IO](t)
        // detectPauses here ???
        .mapParallelBatch(
          /*batchSize = jsonReadAhead / sys.runtime.availableProcessors,
          responsive = true*/)(
          positionAndLine =>
            (positionAndLine.position,
              positionAndLine.value,
              positionAndLine.value.parseJson.flatMap(S.decodeJournalJson).orThrow))
        .filter(testHeartbeatSuppressor) // for testing
        .detectPauses(setting.timing.activeLostTimeout)
        .flatMap[IO, Checked[Unit]]:
          case Left(noHeartbeatSince) =>
            (if isReplicatingHeadOfFile then continuation.clusterState else builder.clusterState) match
              case clusterState: Coupled if clusterState.passiveId == ownId =>
                if awaitingCoupledEvent then
                  logger.trace(
                    s"Ignoring observed pause of ${noHeartbeatSince.elapsed.pretty} without heartbeat " +
                      s"because cluster is coupled but nodes have not yet recoupled: clusterState=$clusterState")
                  Stream.empty  // Ignore
                else
                  logger.warn(s"❗ No heartbeat from the currently active cluster $activeId " +
                    s"since ${noHeartbeatSince.elapsed.pretty} - trying to fail-over")
                  Stream.eval(
                    if isReplicatingHeadOfFile then
                      val recoveredJournalFile = continuation.maybeRecoveredJournalFile.getOrElse(
                        throw new IllegalStateException("Failover but nothing has been replicated"))
                      val lastEventId = recoveredJournalFile.eventId
                      eventIdGenerator.updateLastEventId(lastEventId)
                      val failedOverStamped = toStampedFailedOver(clusterState,
                        JournalPosition(recoveredJournalFile.fileEventId, lastProperEventPosition))
                      val failedOver = failedOverStamped.value.event
                      common.ifClusterWatchAllowsActivation(clusterState, failedOver):
                        IO:
                          val file = recoveredJournalFile.file
                          val fileSize =
                            autoClosing(FileChannel.open(file, APPEND)): out =>
                              writeFailedOverEvent(out, file, failedOverStamped, lastProperEventPosition)
                              out.size
                          builder.startWithState(JournalProgress.InCommittedEventsSection,
                            journalHeader = Some(recoveredJournalFile.journalHeader),
                            eventId = lastEventId,
                            totalEventCount = recoveredJournalFile.nextJournalHeader.totalEventCount + 1,
                            recoveredJournalFile.state)
                          builder.put(failedOverStamped)

                          replicatedFirstEventPosition := recoveredJournalFile.firstEventPosition
                          replicatedFileLength = fileSize
                          lastProperEventPosition = fileSize

                          //eventWatch.onJournalingStarted(???)
                          eventWatch.onFileWrittenAndEventsCommitted(
                            PositionAnd(fileSize, failedOverStamped.eventId), n = 1)
                          eventWatch.onJournalingEnded(fileSize)
                          Right(true)
                    else
                      // TODO Similar to then-part
                      val failedOverStamped = toStampedFailedOver(clusterState,
                        JournalPosition(continuation.fileEventId, lastProperEventPosition))
                      val failedOver = failedOverStamped.value.event
                      common.ifClusterWatchAllowsActivation(clusterState, failedOver):
                        IO:
                          writeFailedOverEvent(out, file, failedOverStamped, lastProperEventPosition)
                          builder.rollbackToEventSection()
                          builder.put(failedOverStamped)
                          val fileSize = out.size
                          replicatedFileLength = fileSize
                          lastProperEventPosition = fileSize
                          eventWatch.onFileWrittenAndEventsCommitted(
                            PositionAnd(fileSize, failedOverStamped.eventId), n = 1)
                          Right(true)
                  ).flatMap:
                    case Left(problem) =>
                      if problem.is(ClusterFailOverWhilePassiveLostProblem)
                        || problem.is(UntaughtClusterWatchProblem)
                        || problem.is(NoClusterWatchProblem) then
                        logger.info(s"No failover because ClusterWatch responded: $problem")
                        Stream.empty   // Ignore
                      else
                        Stream.raiseError[IO](problem.throwable.appendCurrentStackTrace)

                    case Right(false) => Stream.empty   // Ignore
                    case Right(true) => Stream.emit(Right(()))  // End observation

              case clusterState =>
                logger.trace("Ignoring observed pause without heartbeat because cluster is not coupled: " +
                  "clusterState=" + clusterState)
                Stream.empty  // Ignore

          case Right((_, h @ StampedHeartbeatByteArray, _)) =>
            // Already logged by PekkoHttpClient:
            //logger.trace(h.utf8String.trim)
            Stream.empty

          case Right((fileLength, JournalSeparators.EndOfJournalFileMarker, _)) =>
            // fileLength may be advanced to end of file when file's last record is truncated
            logger.debug("End of replicated journal file reached: " +
              s"${file.getFileName} eventId=${builder.eventId} fileLength=$fileLength")
            _eof = true
            Stream.emit(Left(EndOfJournalFileMarker))

          case Right((fileLength, line, journalRecord)) =>
            out.write(line.toByteBuffer)
            // Already logged by PekkoHttpClient:
            //logger.trace(s"Replicated ${continuation.fileEventId}:$fileLength " +
            //  s"${line.utf8StringTruncateAt(200).trim}")
            val isSnapshotTaken = isReplicatingHeadOfFile && journalRecord.match
              case Stamped(_, _, KeyedEvent(_, _: SnapshotTaken)) => true
              case _ => false
            if isSnapshotTaken then
              ensureEqualState(continuation, builder.result())
            builder.put(journalRecord)  // throws on invalid event
            if isSnapshotTaken then
              for tmpFile <- maybeTmpFile do
                val journalId = builder.fileJournalHeader.map(_.journalId) getOrElse
                  sys.error(s"Missing JournalHeader in replicated journal file '$file'")
                for o <- continuation.maybeJournalId if o != journalId do
                  sys.error(s"Received JournalId '$journalId' does not match expected '$o'")
                replicatedFirstEventPosition := replicatedFileLength
                // SnapshotTaken occurs only as the first event of a journal file, just behind the snapshot
                isReplicatingHeadOfFile = false
                out.close()
                move(tmpFile, file, ATOMIC_MOVE)
                journalLocation.updateSymbolicLink(file)
                logger.info(s"Snapshot '${file.getFileName}' (${
                  EventId.toString(continuation.fileEventId)}) replicated - ${
                  bytesPerSecondString(startedAt.elapsed, size(file))}")
                out = FileChannel.open(file, APPEND)
                // replicatedLength is between EventHeader and SnapshotTaken
                eventWatch.onJournalingStarted(file, journalId,
                  firstEventPositionAndFileEventId = PositionAnd(replicatedFileLength, continuation.fileEventId),
                  flushedLengthAndEventId = PositionAnd(fileLength, builder.eventId),
                  isActiveNode = false)
                releaseEvents()
            //assertThat(fileLength == out.size, s"fileLength=$fileLength, out.size=${out.size}")  // Maybe slow
            replicatedFileLength = fileLength
            if builder.journalProgress == JournalProgress.InCommittedEventsSection then
              lastProperEventPosition = fileLength
            if isReplicatingHeadOfFile then
              Stream.emit(Right(()))
            else
              if builder.journalProgress == JournalProgress.InCommittedEventsSection then
                // An open transaction may be rolled back, so we do not notify about these
                eventWatch.onFileWritten(fileLength)
                journalRecord match
                  case Stamped(eventId, _, _) => eventWatch.onEventsCommitted(PositionAnd(fileLength, eventId), 1)
                  case _ =>
              journalRecord match
                case JournalSeparators.Commit =>
                  eventWatch.onEventsCommitted(PositionAnd(fileLength, builder.eventId), 1)
                  Stream.emit(Right(()))

                case Stamped(_, _, KeyedEvent(_, event)) =>
                  event match
                    case _: JournalEventsReleased =>
                      releaseEvents()
                      Stream.emit(Right(()))

                    case clusterEvent: ClusterEvent =>
                      // TODO Use JournalLogging
                      logger.info(clusterEventAndStateToString(clusterEvent, builder.clusterState))
                      clusterEvent match
                        case _: ClusterNodesAppointed | _: ClusterPassiveLost | _: ClusterActiveNodeRestarted =>
                          Stream.eval(
                            tryEndlesslyToSendClusterPrepareCoupling
                              .map(Right.apply))  // TODO Handle heartbeat timeout !

                        case _: ClusterFailedOver =>
                          // Now, this node has switched from still-active (but failed for the other node) to passive.
                          // It's time to recouple.
                          // ClusterPrepareCoupling command requests an event acknowledgement.
                          // To avoid a deadlock, we send ClusterPrepareCoupling command asynchronously and
                          // continue immediately with acknowledgement of ClusterEvent.ClusterCoupled.
                          if !otherFailed then
                            logger.error("Replicated unexpected FailedOver event")  // Should not happen
                          dontActivateBecauseOtherFailedOver = false
                          Stream.eval(
                            tryEndlesslyToSendClusterPrepareCoupling
                              .map(Right.apply))  // TODO Handle heartbeat timeout !

                        case switchedOver: ClusterSwitchedOver =>
                          // Notify ClusterWatch before starting heartbeating
                          val clusterState = builder.clusterState.asInstanceOf[ClusterState.HasNodes]
                          Stream.eval(common
                            .clusterWatchSynchronizer(clusterState)
                            .flatMap(_.applyEvent(switchedOver, clusterState))
                            .map(_.toUnit))

                        case ClusterCouplingPrepared(activeId) =>
                          assertThat(activeId != ownId)
                          Stream.eval(
                            sendClusterCouple
                              .map(Right.apply))  // TODO Handle heartbeat timeout !

                        case ClusterCoupled(activeId) =>
                          assertThat(activeId != ownId)
                          awaitingCoupledEvent = false
                          releaseEvents()
                          Stream.emit(Right(()))

                        case ClusterResetStarted =>
                          assertThat(activeId != ownId)
                          Stream.eval(IO
                            .sleep(1.s) // Allow event acknowledgment !!!
                            .as(Left(PassiveClusterNodeResetProblem)))

                        case _ =>
                          Stream.emit(Right(()))
                    case _ =>
                      Stream.emit(Right(()))
                case _ =>
                  Stream.emit(Right(()))
        .takeWhile(_.left.forall(_ ne EndOfJournalFileMarker))
        .takeThrough(_ => !shouldActivate(builder.clusterState))
        .recoverWith:
          case t: org.apache.pekko.stream.StreamTcpException
            if shouldActivate(builder.clusterState) =>
            // After a ClusterSwitchedOver event has been processed, the stream may fail due
            // to connection reset. Maybe due to a prefetch. We ignore this.
            Stream.exec(IO:
              logger.debug(s"❓ Ignore error due to activation: ${t.toStringWithCauses}"))
        .collect:
          case Left(problem) => problem
          //case Right(Completed) => -ignore-
        .head.compile.last
        .flatMap:
          case Some(problem) => IO.left(problem)
          case None =>
            IO.defer:
              logger.debug(s"replicateJournalFile finished, " +
                s"isReplicatingHeadOfFile=$isReplicatingHeadOfFile, " +
                s"replicatedFileLength=$replicatedFileLength, clusterState=${builder.clusterState}")
              if !isReplicatingHeadOfFile then
                // In case of fail-over while the next journal file's snapshot is written,
                // we need to remove the next file and cut this file's open transaction
                // So we cut off open transaction already now.
                out.truncate(lastProperEventPosition)
                eventWatch.onJournalingEnded(lastProperEventPosition)
              (builder.fileJournalHeader, builder.nextJournalHeader) match
                case (Some(header), Some(nextHeader)) =>
                  IO.right(NextFile(
                    RecoveredJournalFile(file, length = replicatedFileLength,
                      lastProperEventPosition = lastProperEventPosition,
                      header, nextHeader, replicatedFirstEventPosition.orThrow, builder.result())))
                case _ =>
                  shutdown.tryGet.map(_.isDefined).map:
                    if _ then
                      Left(PassiveClusterNodeShutdownProblem)
                    else
                      Left(Problem.pure("JournalHeader could not be replicated " +
                        s"fileEventId=${continuation.fileEventId} eventId=${builder.eventId}"))
        .guarantee(IO:
          out.close())

  private def writeFailedOverEvent(
    out: FileChannel,
    file: Path,
    failedOverStamped: Stamped[KeyedEvent[ClusterFailedOver]],
    lastProperEventPosition: Long)
  : Unit =
    eventWatch.onFailover()
    logger.warn("❗️Failover")
    if out.size > lastProperEventPosition then
      logger.info(s"Truncating open transaction in ${
        file.getFileName}' file at position $lastProperEventPosition")
      out.truncate(lastProperEventPosition)
    // TODO Use JournalLogger
    val event = failedOverStamped: Stamped[KeyedEvent[ClusterEvent]]
    out.write(ByteBuffer.wrap(
      (event.asJson.compactPrint + '\n').getBytes(UTF_8)))
    //out.force(true)  // sync()

  private def testHeartbeatSuppressor(tuple: (Long, ByteArray, Any)): Boolean =
    tuple match
      case (_, StampedHeartbeatByteArray, _)
      if clusterConf.testHeartbeatLossPropertyKey.fold(false)(k => sys.props(k).toBoolean) =>
        logger.warn("TEST: Suppressing the received heartbeat")
        false
      case _ => true

  private def shouldActivate(clusterState: ClusterState) =
    !dontActivateBecauseOtherFailedOver && clusterState.isNonEmptyActive(ownId)

  private def ensureEqualState(continuation: Continuation.Replicatable, snapshot: S): Unit =
    for recoveredJournalFile <- continuation.maybeRecoveredJournalFile do
      if recoveredJournalFile.state != snapshot then
        val msg = s"Calculated '$S' from recovered or replicated journal file ${
          recoveredJournalFile.fileEventId} does not match snapshot in next replicated journal file"
        logger.error(msg)
        // msg may get very big
        //logger.info(msg)  // Without colors because msg is already colored
        SnapshotableState.showDifference(
          recoveredJournalFile.state, "recoveredJournalFile.state",
          snapshot, "snapshot")
        sys.error(msg)

  private def toStampedFailedOver(clusterState: Coupled, failedAt: JournalPosition)
  : Stamped[KeyedEvent[ClusterFailedOver]] =
    val failedOver = ClusterFailedOver(failedActiveId = clusterState.activeId, activatedId = clusterState.passiveId, failedAt)
    val stamped = eventIdGenerator.stamp(NoKey <-: failedOver)
    logger.debug(stamped.toString)
    stamped

  private sealed trait Continuation

  private object Continuation:
    private[PassiveClusterNode] sealed trait Replicatable
    extends Continuation:
      def clusterState: ClusterState
      def fileEventId: EventId
      def fileLength: Long
      def firstEventPosition: Option[Long]
      def lastProperEventPosition: Long
      def maybeJournalId: Option[JournalId]
      def maybeRecoveredJournalFile: Option[RecoveredJournalFile[S]]
      final lazy val file = journalLocation.file(fileEventId)

    private[PassiveClusterNode] sealed trait HasRecoveredJournalFile
    extends Continuation.Replicatable:
      def recoveredJournalFile: RecoveredJournalFile[S]
      def clusterState = recoveredJournalFile.state.clusterState
      final def maybeJournalId = Some(recoveredJournalFile.journalId)
      final def maybeRecoveredJournalFile = Some(recoveredJournalFile)

  private sealed case class NoLocalJournal(fileEventId: EventId)
  extends Continuation.Replicatable:
    def clusterState = ClusterState.Empty
    def fileLength = 0
    def firstEventPosition = None
    def lastProperEventPosition = -1L  // Invalid value
    def maybeJournalId = None
    def maybeRecoveredJournalFile = None

  private sealed case class FirstPartialFile(recoveredJournalFile: RecoveredJournalFile[S])
  extends Continuation.Replicatable, Continuation.HasRecoveredJournalFile:
    assertThat(recoveredJournalFile.file == file)
    def fileLength = recoveredJournalFile.length
    def fileEventId = recoveredJournalFile.fileEventId
    def firstEventPosition = Some(recoveredJournalFile.firstEventPosition)
    def lastProperEventPosition = recoveredJournalFile.lastProperEventPosition
    override def toString = s"FirstPartialFile($fileEventId,$fileLength,${recoveredJournalFile.eventId})"

  private sealed case class NextFile(recoveredJournalFile: RecoveredJournalFile[S])
  extends Continuation.Replicatable, Continuation.HasRecoveredJournalFile:
    /** The next file is initially empty. */
    def fileLength = 0
    /** The next file's EventId is the recovered file's last EventId. */
    def fileEventId = recoveredJournalFile.eventId
    def firstEventPosition = None
    def lastProperEventPosition = recoveredJournalFile.lastProperEventPosition
    override def toString = s"NextFile(${recoveredJournalFile.eventId})"


object PassiveClusterNode:
  private val TmpSuffix = ".tmp"  // Duplicate in JournalActor
  private val logger = Logger[this.type]

  private val EndOfJournalFileMarker = Problem.pure("End of journal file (internal use only)")
  private val PassiveClusterNodeShutdownProblem = Problem("PassiveClusterNode has been shut down")
