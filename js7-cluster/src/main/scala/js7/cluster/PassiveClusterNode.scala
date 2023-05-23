package js7.cluster

import cats.effect.concurrent.Deferred
import com.softwaremill.diffx
import com.typesafe.config.Config
import io.circe.syntax.*
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{exists, move, size}
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.StandardOpenOption.{APPEND, CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Path, Paths}
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.{RichMonixObservable, RichMonixTask}
import js7.base.monixutils.MonixDeadline.now
import js7.base.monixutils.ObservablePauseDetector.RichPauseObservable
import js7.base.monixutils.RefCountedResource
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.utils.StackTraces.*
import js7.base.web.HttpClient
import js7.cluster.ClusterCommon.clusterEventAndStateToString
import js7.cluster.PassiveClusterNode.*
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, NoClusterWatchProblem, UntaughtClusterWatchProblem}
import js7.common.http.RecouplingStreamReader
import js7.common.jsonseq.PositionAnd
import js7.data.cluster.ClusterCommand.{ClusterCouple, ClusterPassiveDown, ClusterPrepareCoupling, ClusterRecouple}
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, IsDecoupled, PreparedToBeCoupled}
import js7.data.cluster.{ClusterEvent, ClusterNodeApi, ClusterSetting, ClusterState}
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.JournalSeparators.HeartbeatMarker
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalId, JournalPosition, JournalSeparators, KeyedEvent, SnapshotableState, SnapshotableStateBuilder, Stamped}
import js7.data.node.NodeId
import js7.journal.EventIdGenerator
import js7.journal.files.JournalFiles.*
import js7.journal.recover.{FileSnapshotableStateBuilder, JournalProgress, Recovered, RecoveredJournalFile}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.annotation.nowarn

private[cluster] final class PassiveClusterNode[S <: SnapshotableState[S]: diffx.Diff](
  ownId: NodeId,
  setting: ClusterSetting,
  recovered: Recovered[S]/*TODO The maybe big SnapshotableState at start sticks here*/,
  eventIdGenerator: EventIdGenerator,
  /** For backup initialization, only when ClusterState.Empty. */
  initialFileEventId: Option[EventId],
  otherFailed: Boolean,
  clusterConf: ClusterConf,
  config: Config,
  common: ClusterCommon)
  (implicit S: SnapshotableState.Companion[S])
{
  import clusterConf.journalConf
  import recovered.{eventWatch, journalMeta}
  import setting.{activeId, idToUri}

  private val jsonReadAhead = config.getInt("js7.web.client.json-read-ahead")
  private val shutdown = Deferred.unsafe[Task, Unit]

  assertThat(activeId != ownId && setting.passiveId == ownId)
  assertThat(initialFileEventId.isDefined == (recovered.clusterState == ClusterState.Empty))

  private val activeApiCache = new RefCountedResource(
    common.clusterNodeApi(idToUri(activeId), "Active node"))

  private def activeApiResource(implicit src: sourcecode.Enclosing) =
    activeApiCache.resource

  private val stateBuilderAndAccessor = new StateBuilderAndAccessor(recovered.state)
  private var dontActivateBecauseOtherFailedOver = otherFailed
  @volatile private var awaitingCoupledEvent = false
  @volatile private var stopped = false

  def onShutdown(dontNotifyActiveNode: Boolean = false): Task[Unit] =
    shutdown.complete(()).attempt *>
      Task.unless(dontNotifyActiveNode)(notifyActiveNodeAboutShutdown)

  /** Allow the active node to emit ClusterPassiveLost quickly. */
  private def notifyActiveNodeAboutShutdown: Task[Unit] =
    logger.debugTask {
      // Active and passive node may be shut down at the same time. We try to handle this here.
      val untilDecoupled = logger.traceTask(Task
        .tailRecM(())(_ =>
          stateBuilderAndAccessor.state
            .map(_.clusterState)
            .flatMap {
              case _: Coupled => Task.left(()).delayResult(1.s)
              case clusterState => Task.right(clusterState)
            }))

      val notifyActive = logger.traceTask(
        activeApiResource.use(api =>
          api.login(onlyIfNotLoggedIn = true)
            .*>(api.executeClusterCommand(ClusterPassiveDown(activeId = activeId, passiveId = ownId)))
            .void
            .onErrorHandle(throwable => logger.debug(
              s"ClusterCommand.ClusterPassiveDown failed: ${throwable.toStringWithCauses}",
              throwable.nullIfNoStackTrace))))
            .timeoutTo(clusterConf.timing.heartbeat/*some short time*/, Task.unit)
        .logWhenItTakesLonger

      Task.race(untilDecoupled, notifyActive.delayExecution(50.ms))
        .tapEval {
          case Left(clusterState) =>
            // The connection of the killed notifyActive HTTP request may be still blocked !!!
            // until it is responded (see AkkaHttpClient)
            // It should not disturb the shutdown.
            Task(logger.debug(s"notifyActiveNodeAboutShutdown: clusterState=$clusterState"))
          case Right(()) =>
            Task(logger.debug(
              "notifyActiveNodeAboutShutdown: Active node has been notified about shutdown"))
        }
        .as(())
    }

  def state: Task[S] =
    stateBuilderAndAccessor.state

  /**
    * Runs the passive node until activated or terminated.
    * Returns also a `Task` with the current ClusterState while being passive or active.
    */
  def run(recoveredState: S): Task[Checked[Recovered[S]]] =
    CorrelId.bindNew(logger.debugTask(
      common.requireValidLicense
        .flatMapT(_ => Task.deferAction { implicit s =>
          val recoveredClusterState = recoveredState.clusterState
          logger.debug(s"recoveredClusterState=$recoveredClusterState")
          assertThat(!stopped)  // Single-use only

          // Delete obsolete journal files left by last run
          if (journalConf.deleteObsoleteFiles) {
            for (f <- recovered.recoveredJournalFile) {
              val eventId = f.fileEventId/*release files before the recovered file*/
              eventWatch.releaseEvents(
                recoveredState.journalState.toReleaseEventId(eventId, journalConf.releaseEventsUserIds))
            }
          }

          for (o <- recovered.recoveredJournalFile) {
            cutJournalFile(o.file, o.length, o.eventId)
          }

          if (!otherFailed) { // Other node failed-over while this node was active but lost? FailedOver event will be replicated.
            recoveredClusterState
              .match_ {
                case ClusterState.Empty =>
                  Task.unit
                case _: IsDecoupled =>
                  tryEndlesslyToSendClusterPrepareCoupling
                case _: PreparedToBeCoupled =>
                  common.tryEndlesslyToSendCommand(
                    activeApiResource,
                    ClusterCouple(activeId = activeId, passiveId = ownId))
                case _: Coupled =>
                  // After a quick restart of this passive node, the active node may not yet have noticed the loss.
                  // So we send a ClusterRecouple command to force a ClusterPassiveLost event.
                  // Then the active node couples again with this passive node,
                  // and we are sure to be coupled and up-to-date and may properly fail-over in case of active node loss.
                  // The active node ignores this command if it has emitted a ClusterPassiveLost event.
                  awaitingCoupledEvent = true
                  common.tryEndlesslyToSendCommand(
                    activeApiResource,
                    ClusterRecouple(activeId = activeId, passiveId = ownId))
              }
              .runAsyncUncancelable {
                case Left(throwable) => logger.error(
                  "While notifying the active cluster node about restart of this passive node:" +
                  s" ${throwable.toStringWithCauses}", throwable.nullIfNoStackTrace)
                case Right(()) =>
                  logger.debug(
                    "Active cluster node has been notified about restart of this passive node")
              }
          }

          replicateJournalFiles(recoveredClusterState)
            .guarantee(Task {
              stopped = true
            })
            .guarantee(activeApiCache.clear)
        })))

  private def cutJournalFile(file: Path, length: Long, eventId: EventId): Unit =
    if (exists(file)) {
      val garbage = size(file) - length
      if (garbage > 0) {
        // Partial event or partial transaction
        logger.info(s"Cutting incomplete data ($garbage bytes) at end of ${file.getFileName} at position $length, EventId $eventId ")
        autoClosing(FileChannel.open(file, WRITE)) { f =>
          f.truncate(length)
        }
      }
    }

  private def tryEndlesslyToSendClusterPrepareCoupling: Task[Unit] = {
    // TODO Delay until we have replicated nearly all events, to avoid a long PreparedCoupled state
    //  Annähernd gleichlaufende Uhren vorausgesetzt, können wir den Zeitstempel des letzten Events heranziehen.
    //  Wenn kein Event kommt? Herzschlag mit Zeitstempel (nicht EventId) versehen (wäre sowieso nützlich)
    //  ["HEARTBEAT", { timestamp: 1234.567 }]
    //  {TYPE: "Heartbeat", eventId: 1234567000, timestamp: 1234.567 }    Herzschlag-Event?
    //  Funktioniert nicht, wenn die Uhren verschieden gehen. Differenz feststellen?
    Task
      .race(
        shutdown.get,
        common.tryEndlesslyToSendCommand(
          activeApiResource,
          ClusterPrepareCoupling(activeId = activeId, passiveId = ownId)))
      .flatMap {
        case Left(()) => Task(logger.debug(
          "⚫ tryEndlesslyToSendClusterPrepareCoupling canceled due to shutdown"))
        case Right(()) => Task.unit
      }
  }

  private def sendClusterCouple: Task[Unit] =
    common.tryEndlesslyToSendCommand(
      activeApiResource,
      ClusterCouple(activeId = activeId, passiveId = ownId))

  private def replicateJournalFiles(recoveredClusterState: ClusterState)
  : Task[Checked[Recovered[S]]] =
    activeApiResource
      .use(activeNodeApi =>
        Task.tailRecM(
          (recovered.recoveredJournalFile match {
            case None =>
              assertThat(recoveredClusterState == ClusterState.Empty)
              NoLocalJournal(initialFileEventId.get)
            case Some(recoveredJournalFile) =>
              FirstPartialFile(recoveredJournalFile/*, recoveredClusterState*/)
          }): Continuation.Replicatable
        )(continuation =>
          Task.deferAction(implicit scheduler =>
            replicateJournalFile(continuation, () => stateBuilderAndAccessor.newStateBuilder(), activeNodeApi)
          ) // TODO Herzschlag auch beim Wechsel zur nächsten Journaldatei prüfen
            .map {
              case Left(problem) =>
                Right(Left(problem))

              case Right(continuation) if shouldActivate(continuation.clusterState) =>
                logger.info(s"Activating because ClusterState has become ${continuation.clusterState}")
                // Replace Recovered (forget the old one, do not close, because JournalEventWatch is the same)
                Right(Right(
                  recovered.changeRecoveredJournalFile(continuation.maybeRecoveredJournalFile)))

              case Right(continuation) =>
                Left(continuation)
            }))

  private def replicateJournalFile(
    continuation: Continuation.Replicatable,
    newStateBuilder: () => SnapshotableStateBuilder[S],
    activeNodeApi: ClusterNodeApi)
    (implicit s: Scheduler)
  : Task[Checked[Continuation.Replicatable]] =
    Task.defer {
      import continuation.file

      val maybeTmpFile = locally {
        // Suppress wrong "unreachable code" error for FirstPartialFile case
        @nowarn("cat=other-match-analysis")
        def toTmpFile = continuation match {
          case _: NoLocalJournal | _: NextFile =>
            val tmp = Paths.get(file.toString + TmpSuffix)
            logger.debug(s"Replicating snapshot into temporary journal file ${tmp.getFileName}")
            Some(tmp)

          case _: FirstPartialFile =>
            None
        }
        toTmpFile
      }

      var out = maybeTmpFile match {
        case None => FileChannel.open(file, APPEND)
        case Some(tmp) => FileChannel.open(tmp, CREATE, WRITE, TRUNCATE_EXISTING)
      }
      var isReplicatingHeadOfFile = maybeTmpFile.isDefined
      val startedAt = now
      val replicatedFirstEventPosition = SetOnce.fromOption(continuation.firstEventPosition, "replicatedFirstEventPosition")
      var replicatedFileLength = continuation.fileLength
      var lastProperEventPosition = continuation.lastProperEventPosition
      var _eof = false
      val builder = new FileSnapshotableStateBuilder(journalFileForInfo = file.getFileName,
        continuation.maybeJournalId, newStateBuilder)

      def releaseEvents(): Unit =
        if (journalConf.deleteObsoleteFiles) {
          eventWatch.releaseEvents(
            builder.journalState
              .toReleaseEventId(eventWatch.lastFileEventId, journalConf.releaseEventsUserIds))
        }

      continuation match {
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
      }

      val recouplingStreamReader = new RecouplingStreamReader[Long/*file position*/, PositionAnd[ByteArray], ClusterNodeApi](
        toIndex = _.position,
        clusterConf.recouplingStreamReader)
      {
        protected def getObservable(api: ClusterNodeApi, position: Long) =
          HttpClient.liftProblem(
            api.journalObservable(
              JournalPosition(continuation.fileEventId, position),
              heartbeat = Some(setting.timing.heartbeat),
              markEOF = true
            ).map(_.scan(PositionAnd(position, ByteArray.empty/*unused*/))((s, line) =>
              PositionAnd(s.position + (if (line == HeartbeatMarker) 0 else line.length), line))))

        protected def stopRequested = stopped

        override def eof(index: Long) = _eof && index >= replicatedFileLength
      }

      locally {
        val f = maybeTmpFile getOrElse file
        logger.trace(s"replicateJournalFile($continuation) size(${f.getFileName})=${size(f)} ${builder.clusterState}")
        assertThat(continuation.fileLength == size(f))
      }

      // TODO Eine Zeile davor lesen und sicherstellen, dass sie gleich unserer letzten Zeile ist
      recouplingStreamReader.observe(activeNodeApi, after = continuation.fileLength)
        // TODO Aktiver kann JournalFileIsNotReady melden, sendet keinen Herzschlag, ist aber irgendwie am Leben.
        //  observe könnte selbst Ausfall des Aktiven anzeigen, gdw er nicht erreichbar ist
        //  (zB Login klappt nicht, isTemporaryUnreachable).
        //  observe überwacht selbst die Herzschläge, und verbindet sich bei Ausfall erneut.
        //  Dann entfällt hier die Herzschlagüberwachung.
        .doOnError(t => Task {
          logger.debug(s"observeJournalFile($activeNodeApi, fileEventId=${continuation.fileEventId}, " +
            s"position=${continuation.fileLength}) failed with ${t.toStringWithCauses}", t)
        })
        // detectPauses here ???
        .mapParallelBatch(
          batchSize = jsonReadAhead / sys.runtime.availableProcessors,
          responsive = true)(
          positionAndLine =>
            (positionAndLine.position,
              positionAndLine.value,
              positionAndLine.value.parseJson.flatMap(S.decodeJournalJson).orThrow))
        .filter(testHeartbeatSuppressor) // for testing
        .detectPauses(setting.timing.activeLostTimeout)
        .flatMap[Checked[Unit]] {
          case Left(noHeartbeatSince) =>
            (if (isReplicatingHeadOfFile) continuation.clusterState else builder.clusterState) match {
              case clusterState: Coupled if clusterState.passiveId == ownId =>
                if (awaitingCoupledEvent) {
                  logger.trace(
                    s"Ignoring observed pause of ${noHeartbeatSince.elapsed.pretty} without heartbeat " +
                      s"because cluster is coupled but nodes have not yet recoupled: clusterState=$clusterState")
                  Observable.empty  // Ignore
                } else {
                  logger.warn(s"❗ No heartbeat from the currently active cluster $activeId " +
                    s"since ${noHeartbeatSince.elapsed.pretty} - trying to fail-over")
                  Observable.fromTask(
                    if (isReplicatingHeadOfFile) {
                      val recoveredJournalFile = continuation.maybeRecoveredJournalFile.getOrElse(
                        throw new IllegalStateException("Failover but nothing has been replicated"))
                      val lastEventId = recoveredJournalFile.eventId
                      eventIdGenerator.updateLastEventId(lastEventId)
                      val failedOverStamped = toStampedFailedOver(clusterState,
                        JournalPosition(recoveredJournalFile.fileEventId, lastProperEventPosition))
                      val failedOver = failedOverStamped.value.event
                      common.ifClusterWatchAllowsActivation(clusterState, failedOver)(
                        Task {
                          logger.warn("❗️Failover")
                          val file = recoveredJournalFile.file
                          val fileSize =
                            autoClosing(FileChannel.open(file, APPEND)) { out =>
                              writeFailedOverEvent(out, file, failedOverStamped, lastProperEventPosition)
                              out.size
                            }
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
                        })
                    } else {
                      // TODO Similar to then-part
                      val failedOverStamped = toStampedFailedOver(clusterState,
                        JournalPosition(continuation.fileEventId, lastProperEventPosition))
                      val failedOver = failedOverStamped.value.event
                      common.ifClusterWatchAllowsActivation(clusterState, failedOver)(
                        Task {
                          logger.warn("❗️Failover")
                          writeFailedOverEvent(out, file, failedOverStamped, lastProperEventPosition)
                          builder.rollbackToEventSection()
                          builder.put(failedOverStamped)
                          val fileSize = out.size
                          replicatedFileLength = fileSize
                          lastProperEventPosition = fileSize
                          eventWatch.onFileWrittenAndEventsCommitted(
                            PositionAnd(fileSize, failedOverStamped.eventId), n = 1)
                          Right(true)
                        })
                    }
                  ).flatMap {
                    case Left(problem) =>
                      if (problem.is(ClusterFailOverWhilePassiveLostProblem)
                        || problem.is(UntaughtClusterWatchProblem)
                        || problem.is(NoClusterWatchProblem)) {
                        logger.info(s"No failover because ClusterWatch responded: $problem")
                        Observable.empty   // Ignore
                      } else
                        Observable.raiseError(problem.throwable.appendCurrentStackTrace)

                    case Right(false) => Observable.empty   // Ignore
                    case Right(true) => Observable.pure(Right(()))  // End observation
                  }
                }

              case clusterState =>
                logger.trace("Ignoring observed pause without heartbeat because cluster is not coupled: " +
                  "clusterState=" + clusterState)
                Observable.empty  // Ignore
            }

          case Right((_, HeartbeatMarker, _)) =>
            logger.trace(HeartbeatMarker.utf8String.trim)
            Observable.empty

          case Right((fileLength, JournalSeparators.EndOfJournalFileMarker, _)) =>
            // fileLength may be advanced to end of file when file's last record is truncated
            logger.debug("End of replicated journal file reached: " +
              s"${file.getFileName} eventId=${builder.eventId} fileLength=$fileLength")
            _eof = true
            Observable.pure(Left(EndOfJournalFileMarker))

          case Right((fileLength, line, journalRecord)) =>
            out.write(line.toByteBuffer)
            logger.trace(s"Replicated ${continuation.fileEventId}:$fileLength " +
              s"${line.utf8StringTruncateAt(200).trim}")
            val isSnapshotTaken = isReplicatingHeadOfFile && (journalRecord match {
              case Stamped(_, _, KeyedEvent(_, _: SnapshotTaken)) => true
              case _ => false
            })
            if (isSnapshotTaken) {
              ensureEqualState(continuation, builder.result())
            }
            builder.put(journalRecord)  // throws on invalid event
            if (isSnapshotTaken) {
              for (tmpFile <- maybeTmpFile) {
                val journalId = builder.fileJournalHeader.map(_.journalId) getOrElse
                  sys.error(s"Missing JournalHeader in replicated journal file '$file'")
                for (o <- continuation.maybeJournalId if o != journalId)
                  sys.error(s"Received JournalId '$journalId' does not match expected '$o'")
                replicatedFirstEventPosition := replicatedFileLength
                // SnapshotTaken occurs only as the first event of a journal file, just behind the snapshot
                isReplicatingHeadOfFile = false
                out.close()
                move(tmpFile, file, ATOMIC_MOVE)
                journalMeta.updateSymbolicLink(file)
                logger.info(s"Snapshot '${file.getFileName}' " +
                  s"(${EventId.toString(continuation.fileEventId)}) replicated - " +
                  bytesPerSecondString(startedAt.elapsed, size(file)))
                out = FileChannel.open(file, APPEND)
                // replicatedLength is between EventHeader and SnapshotTaken
                eventWatch.onJournalingStarted(file, journalId,
                  firstEventPositionAndFileEventId = PositionAnd(replicatedFileLength, continuation.fileEventId),
                  flushedLengthAndEventId = PositionAnd(fileLength, builder.eventId),
                  isActiveNode = false)
                releaseEvents()
              }
            }
            //assertThat(fileLength == out.size, s"fileLength=$fileLength, out.size=${out.size}")  // Maybe slow
            replicatedFileLength = fileLength
            if (builder.journalProgress == JournalProgress.InCommittedEventsSection) {
              lastProperEventPosition = fileLength
            }
            if (isReplicatingHeadOfFile)
              Observable.pure(Right(()))
            else {
              if (builder.journalProgress == JournalProgress.InCommittedEventsSection) {
                // An open transaction may be rolled back, so we do not notify about these
                eventWatch.onFileWritten(fileLength)
                journalRecord match {
                  case Stamped(eventId, _, _) => eventWatch.onEventsCommitted(PositionAnd(fileLength, eventId), 1)
                  case _ =>
                }
              }
              journalRecord match {
                case JournalSeparators.Commit =>
                  eventWatch.onEventsCommitted(PositionAnd(fileLength, builder.eventId), 1)
                  Observable.pure(Right(()))

                case Stamped(_, _, KeyedEvent(_, event)) =>
                  event match {
                    case _: JournalEventsReleased =>
                      releaseEvents()
                      Observable.pure(Right(()))

                    case clusterEvent: ClusterEvent =>
                      // TODO Use JournalLogging
                      logger.info(clusterEventAndStateToString(clusterEvent, builder.clusterState))
                      clusterEvent match {
                        case _: ClusterNodesAppointed | _: ClusterPassiveLost | _: ClusterActiveNodeRestarted =>
                          Observable.fromTask(
                            tryEndlesslyToSendClusterPrepareCoupling
                              .map(Right.apply))  // TODO Handle heartbeat timeout !

                        case _: ClusterFailedOver =>
                          // Now, this node has switched from still-active (but failed for the other node) to passive.
                          // It's time to recouple.
                          // ClusterPrepareCoupling command requests an event acknowledgement.
                          // To avoid a deadlock, we send ClusterPrepareCoupling command asynchronously and
                          // continue immediately with acknowledgement of ClusterEvent.ClusterCoupled.
                          if (!otherFailed)
                            logger.error("Replicated unexpected FailedOver event")  // Should not happen
                          dontActivateBecauseOtherFailedOver = false
                          Observable.fromTask(
                            tryEndlesslyToSendClusterPrepareCoupling
                              .map(Right.apply))  // TODO Handle heartbeat timeout !

                        case switchedOver: ClusterSwitchedOver =>
                          // Notify ClusterWatch before starting heartbeating
                          val clusterState = builder.clusterState.asInstanceOf[ClusterState.HasNodes]
                          Observable.fromTask(common
                            .clusterWatchSynchronizer(clusterState)
                            .flatMap(_.applyEvent(switchedOver, clusterState))
                            .map(_.toUnit))

                        case ClusterCouplingPrepared(activeId) =>
                          assertThat(activeId != ownId)
                          Observable.fromTask(
                            sendClusterCouple
                              .map(Right.apply))  // TODO Handle heartbeat timeout !

                        case ClusterCoupled(activeId) =>
                          assertThat(activeId != ownId)
                          awaitingCoupledEvent = false
                          releaseEvents()
                          Observable.pure(Right(()))

                        case _ =>
                          Observable.pure(Right(()))
                      }
                    case _ =>
                      Observable.pure(Right(()))
                  }
                case _ =>
                  Observable.pure(Right(()))
              }
            }
        }
        .takeWhile(_.left.forall(_ ne EndOfJournalFileMarker))
        .takeWhileInclusive(_ => !shouldActivate(builder.clusterState))
        .collect {
          case Left(problem) => problem
          //case Right(Completed) => -ignore-
        }
        .headOptionL
        .map {
          case Some(problem) => Left(problem)
          case None =>
            logger.debug(s"replicateJournalFile(${file.getFileName}) finished, " +
              s"isReplicatingHeadOfFile=$isReplicatingHeadOfFile, " +
              s"replicatedFileLength=$replicatedFileLength, clusterState=${builder.clusterState}")
            if (!isReplicatingHeadOfFile) {
              // In case of fail-over while the next journal file's snapshot is written,
              // we need to remove the next file and cut this file's open transaction
              // So we cut off open transaction already now.
              out.truncate(lastProperEventPosition)
              eventWatch.onJournalingEnded(lastProperEventPosition)
            }
            (builder.fileJournalHeader, builder.nextJournalHeader) match {
              case (Some(header), Some(nextHeader)) =>
                Right(NextFile(
                  RecoveredJournalFile(file, length = replicatedFileLength,
                    lastProperEventPosition = lastProperEventPosition,
                    header, nextHeader, replicatedFirstEventPosition.orThrow, builder.result())))
              case _ =>
                Left(Problem.pure("JournalHeader could not be replicated " +
                  s"fileEventId=${continuation.fileEventId} eventId=${builder.eventId}"))
            }
        }
        .guarantee(
          Task { out.close() })
    }

  private def writeFailedOverEvent(
    out: FileChannel,
    file: Path,
    failedOverStamped: Stamped[KeyedEvent[ClusterFailedOver]],
    lastProperEventPosition: Long)
  : Unit = {
    if (out.size > lastProperEventPosition) {
      logger.info(s"Truncating open transaction in ${
        file.getFileName}' file at position $lastProperEventPosition")
      out.truncate(lastProperEventPosition)
    }
    // TODO Use JournalLogger
    val event = failedOverStamped: Stamped[KeyedEvent[ClusterEvent]]
    out.write(ByteBuffer.wrap(
      (event.asJson.compactPrint + '\n').getBytes(UTF_8)))
    //out.force(true)  // sync()
  }

  private def testHeartbeatSuppressor(tuple: (Long, ByteArray, Any)): Boolean =
    tuple match {
      case (_, HeartbeatMarker, _)
      if clusterConf.testHeartbeatLossPropertyKey.fold(false)(k => sys.props(k).toBoolean) =>
        logger.warn("TEST: Suppressing received heartbeat")
        false
      case _ => true
    }

  private def shouldActivate(clusterState: ClusterState) =
    !dontActivateBecauseOtherFailedOver && clusterState.isNonEmptyActive(ownId)

  private def ensureEqualState(continuation: Continuation.Replicatable, snapshot: S): Unit =
    for (recoveredJournalFile <- continuation.maybeRecoveredJournalFile if recoveredJournalFile.state != snapshot) {
      var msg = s"Calculated '$S' from recovered or replicated journal file" +
        s" ${recoveredJournalFile.fileEventId} does not match snapshot in next replicated journal file"
      // msg may get very big
      logger.error(msg)
      msg ++= ":\n" ++ diffx.compare(recoveredJournalFile.state, snapshot).show()
      logger.info(msg)  // Without colors because msg is already colored
      sys.error(msg)
    }

  private def toStampedFailedOver(clusterState: Coupled, failedAt: JournalPosition)
  : Stamped[KeyedEvent[ClusterFailedOver]] = {
    val failedOver = ClusterFailedOver(failedActiveId = clusterState.activeId, activatedId = clusterState.passiveId, failedAt)
    val stamped = eventIdGenerator.stamp(NoKey <-: failedOver)
    logger.debug(stamped.toString)
    stamped
  }

  private sealed trait Continuation

  private object Continuation {
    private[PassiveClusterNode] sealed trait Replicatable
    extends Continuation {
      def clusterState: ClusterState
      def fileEventId: EventId
      def fileLength: Long
      def firstEventPosition: Option[Long]
      def lastProperEventPosition: Long
      def maybeJournalId: Option[JournalId]
      def maybeRecoveredJournalFile: Option[RecoveredJournalFile[S]]
      final lazy val file = journalMeta.file(fileEventId)
    }

    private[PassiveClusterNode] sealed trait HasRecoveredJournalFile
    extends Continuation.Replicatable {
      def recoveredJournalFile: RecoveredJournalFile[S]
      def clusterState = recoveredJournalFile.state.clusterState
      final def maybeJournalId = Some(recoveredJournalFile.journalId)
      final def maybeRecoveredJournalFile = Some(recoveredJournalFile)
    }
  }

  private sealed case class NoLocalJournal(fileEventId: EventId)
  extends Continuation.Replicatable {
    def clusterState = ClusterState.Empty
    def fileLength = 0
    def firstEventPosition = None
    def lastProperEventPosition = -1L  // Invalid value
    def maybeJournalId = None
    def maybeRecoveredJournalFile = None
  }

  private sealed case class FirstPartialFile(recoveredJournalFile: RecoveredJournalFile[S])
  extends Continuation.Replicatable with Continuation.HasRecoveredJournalFile {
    assertThat(recoveredJournalFile.file == file)
    def fileLength = recoveredJournalFile.length
    def fileEventId = recoveredJournalFile.fileEventId
    def firstEventPosition = Some(recoveredJournalFile.firstEventPosition)
    def lastProperEventPosition = recoveredJournalFile.lastProperEventPosition
    override def toString = s"FirstPartialFile($fileEventId,$fileLength,${recoveredJournalFile.eventId})"
  }

  private sealed case class NextFile(recoveredJournalFile: RecoveredJournalFile[S])
  extends Continuation.Replicatable with Continuation.HasRecoveredJournalFile {
    /** The next file is initially empty. */
    def fileLength = 0
    /** The next file's EventId is the recovered file's last EventId. */
    def fileEventId = recoveredJournalFile.eventId
    def firstEventPosition = None
    def lastProperEventPosition = recoveredJournalFile.lastProperEventPosition
    override def toString = s"NextFile(${recoveredJournalFile.eventId})"
  }
}

object PassiveClusterNode
{
  private val TmpSuffix = ".tmp"  // Duplicate in JournalActor
  private val logger = Logger(getClass)

  private val EndOfJournalFileMarker = Problem.pure("End of journal file (internal use only)")
}
