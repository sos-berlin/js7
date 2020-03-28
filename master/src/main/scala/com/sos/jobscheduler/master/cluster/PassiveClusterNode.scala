package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.ScodecUtils.RichByteVector
import com.sos.jobscheduler.common.event.{EventIdGenerator, PositionAnd}
import com.sos.jobscheduler.common.http.{AkkaHttpClient, RecouplingStreamReader}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.core.event.journal.data.{JournalMeta, JournalSeparators}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles._
import com.sos.jobscheduler.core.event.journal.recover.{FileJournaledStateBuilder, JournalProgress, Recovered, RecoveredJournalFile}
import com.sos.jobscheduler.core.event.state.JournaledStateBuilder
import com.sos.jobscheduler.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState.{Coupled, Decoupled, NodesAppointed, PreparedToBeCoupled}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.JournalEvent.SnapshotTaken
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, JournalEvent, JournalId, JournalPosition, JournaledState, KeyedEvent, Stamped}
import com.sos.jobscheduler.master.client.HttpMasterApi
import com.sos.jobscheduler.master.cluster.ObservablePauseDetector.RichPauseObservable
import com.sos.jobscheduler.master.cluster.PassiveClusterNode._
import com.sos.jobscheduler.master.data.MasterCommand.{ClusterCouple, ClusterPrepareCoupling, ClusterRecouple}
import io.circe.Json
import io.circe.syntax._
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{exists, move, size}
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.StandardOpenOption.{APPEND, CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Path, Paths}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.TimeoutException
import scodec.bits.ByteVector

/*private[cluster]*/ final class PassiveClusterNode[S <: JournaledState[S, Event]](
  ownId: ClusterNodeId,
  idToUri: Map[ClusterNodeId, Uri],
  activeId: ClusterNodeId,
  journalMeta: JournalMeta,
  recovered: Recovered[S, Event],
  otherFailed: Boolean,
  clusterConf: ClusterConf,
  eventIdGenerator: EventIdGenerator,
  common: ClusterCommon)
  (implicit actorSystem: ActorSystem)
{
  assertThat(activeId != ownId)
  import recovered.eventWatch
  private val activeUri = idToUri(activeId)

  private val stateBuilderAndAccessor = new StateBuilderAndAccessor[S, Event](recovered.newStateBuilder)
  private var dontActivateBecauseOtherFailedOver = otherFailed
  @volatile var awaitingCoupledEvent = false
  @volatile private var stopped = false

  def state: Task[S] =
    stateBuilderAndAccessor.state

  /**
    * Runs the passive mode until being activated or terminated.
    * Returns also a `Task` with the current ClusterState while being passive or active.
    * @param recoveredClusterState After failover, this is the other (failed-over) node's ClusterState
    * @param recoveredState
    * @return
    */
  def run(recoveredClusterState: ClusterState, recoveredState: S)
  : Task[Checked[(ClusterState, ClusterFollowUp[S, Event])]] =
    Task.deferAction { implicit s =>
      logger.debug(s"recoveredClusterState=$recoveredClusterState")
      assertThat(!stopped)  // Single-use only
      for (o <- recovered.recoveredJournalFile) {
        cutJournalFile(o.file, o.length, o.eventId)
      }
      val sendCommand =
        if (otherFailed)
          // Other node failed-over while this node was active but lost. FailedOver event will be replicated.
          Task.unit
        else
          recoveredClusterState match {
            case ClusterState.Empty =>
              Task.unit
            case _: NodesAppointed | _: Decoupled =>
              sendClusterPrepareCoupling
            case _: PreparedToBeCoupled =>
              common.tryEndlesslyToSendCommand(activeUri, ClusterCouple(activeId = activeId, passiveId = ownId))
            case _: Coupled =>
              // After a quick restart of this passive node, the active node may not yet have noticed the loss.
              // So we send a ClusterRecouple command to force a PassiveLost event.
              // Then the active node couples again with this passive node,
              // and we are sure to be coupled and up-to-date and may properly fail-over in case of an active node.
              // The active node ignores this command if it has issued an PassiveLost event.
              awaitingCoupledEvent = true
              common.tryEndlesslyToSendCommand(activeUri, ClusterRecouple(activeId = activeId, passiveId = ownId))
          }
      sendCommand
        .onErrorRecover { case t: Throwable =>
          logger.warn(s"Sending Cluster command to other node failed: $t", t)
        }.runAsyncAndForget
      replicateJournalFiles(recoveredClusterState)
        .guarantee(Task {
          stopped = true
        })
    }

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

  private def sendClusterPrepareCoupling: Task[Unit] =
    common.tryEndlesslyToSendCommand(activeUri, ClusterPrepareCoupling(activeId = activeId, passiveId = ownId))

  private def sendClusterCouple: Task[Unit] =
    common.tryEndlesslyToSendCommand(activeUri, ClusterCouple(activeId = activeId, passiveId = ownId))

  private def replicateJournalFiles(recoveredClusterState: ClusterState)
  : Task[Checked[(ClusterState, ClusterFollowUp[S, Event])]] =
    common.masterApi(activeUri, "journal")
      .use(activeMasterApi =>
        Task.tailRecM(
          (recovered.recoveredJournalFile match {
            case None => NoLocalJournal(recoveredClusterState)
            case Some(recoveredJournalFile) => FirstPartialFile(recoveredJournalFile, recoveredClusterState)
          }): Continuation.Replicatable
        )(continuation =>
          Task.deferAction(implicit scheduler =>
            replicateJournalFile(continuation, () => stateBuilderAndAccessor.newStateBuilder(), activeMasterApi)
          ) // TODO Herzschlag auch beim Wechsel zur nächsten Journaldatei prüfen
            .map {
              case Left(problem) =>
                Right(Left(problem))

              case Right(continuation) if shouldActivate(continuation.clusterState) =>
                logger.info(s"Activating because ClusterState has become ${continuation.clusterState}")
                Right(Right((
                  continuation.clusterState,
                  ClusterFollowUp.BecomeActive(
                    recovered.copy[S, Event](recoveredJournalFile = continuation.maybeRecoveredJournalFile)))))

              case Right(continuation) =>
                Left(continuation)
            }))

  private def replicateJournalFile(
    continuation: Continuation.Replicatable,
    newStateBuilder: () => JournaledStateBuilder[S, Event],
    activeMasterApi: HttpMasterApi)
  : Task[Checked[Continuation.Replicatable]] =
    Task.defer {
      import continuation.file

      val maybeTmpFile = continuation match {
        case _: NoLocalJournal | _: NextFile =>
          val tmp = Paths.get(file.toString + TmpSuffix)
          logger.debug(s"Replicating snapshot into temporary journal file ${tmp.getName()}")
          Some(tmp)

        case _: FirstPartialFile =>
          None
      }

      var out = maybeTmpFile match {
        case None => FileChannel.open(file, APPEND)
        case Some(tmp) => FileChannel.open(tmp, CREATE, WRITE, TRUNCATE_EXISTING)
      }
      var isReplicatingHeadOfFile = maybeTmpFile.isDefined
      val replicatedFirstEventPosition = SetOnce.fromOption(continuation.firstEventPosition, "replicatedFirstEventPosition")
      var replicatedFileLength = continuation.fileLength
      var lastProperEventPosition = continuation.lastProperEventPosition
      var _eof = false
      val builder = new FileJournaledStateBuilder[S, Event](journalMeta, journalFileForInfo = file.getFileName,
        continuation.maybeJournalId, newStateBuilder)

      continuation match {
        case FirstPartialFile(recoveredJournalFile, _) =>
          logger.info(s"Start replicating events into journal file ${file.getName()}")
          builder.startWithState(JournalProgress.InCommittedEventsSection, Some(recoveredJournalFile.journalHeader),
            eventId = recoveredJournalFile.eventId,
            totalEventCount = recoveredJournalFile.calculatedJournalHeader.totalEventCount,
            recoveredJournalFile.state)
          eventWatch.onJournalingStarted(file,
            recoveredJournalFile.journalId,
            tornLengthAndEventId = PositionAnd(recoveredJournalFile.firstEventPosition, continuation.fileEventId),
            flushedLengthAndEventId = PositionAnd(recoveredJournalFile.length, recoveredJournalFile.eventId))

        case _ =>
      }

      val recouplingStreamReader = new RecouplingStreamReader[Long/*file position*/, PositionAnd[ByteVector], HttpMasterApi](
        toIndex = _.position,
        //api,
        clusterConf.userAndPassword,
        clusterConf.recouplingStreamReader)
      {
        protected def getObservable(api: HttpMasterApi, after: EventId) =
          AkkaHttpClient.liftProblem(
            api.journalObservable(
              fileEventId = continuation.fileEventId,
              position = after,
              heartbeat = Some(clusterConf.heartbeat),
              markEOF = true
            ).map(_.scan(PositionAnd(after, ByteVector.empty/*unused*/))((s, line) =>
              PositionAnd(s.position + (if (line == JournalSeparators.HeartbeatMarker) 0 else line.length), line))))

        protected def stopRequested = stopped

        override def eof(index: Long) = _eof && index >= replicatedFileLength
      }

      locally {
        val f = maybeTmpFile getOrElse file
        logger.trace(s"replicateJournalFile($continuation) size(${f.getName()})=${size(f)} ${builder.clusterState}")
        assertThat(continuation.fileLength == size(f))
      }

      // TODO Eine Zeile davor lesen und sicherstellen, dass sie gleich unserer letzten Zeile ist
      recouplingStreamReader.observe(activeMasterApi, after = continuation.fileLength)
        .doOnError(t => Task {
          logger.debug(s"observeJournalFile($activeMasterApi, fileEventId=${continuation.fileEventId}, " +
            s"position=${continuation.fileLength}) failed with ${t.toStringWithCauses}", t)
        })
        .mapParallelOrdered(sys.runtime.availableProcessors) { case PositionAnd(fileLength, line) =>
          Task((fileLength, line, line.parseJson.orThrow))
        }
        .filter(testHeartbeatSuppressor) // for testing
        .detectPauses(clusterConf.heartbeat + clusterConf.failAfter)
        .flatMap[Checked[Unit]] {
          case None/*heartbeat pause*/ =>
            (if (isReplicatingHeadOfFile) continuation.clusterState else builder.clusterState) match {
              case clusterState: Coupled if clusterState.passiveId == ownId =>
                if (awaitingCoupledEvent) {
                  logger.trace(
                    s"Ignoring observed pause without heartbeat because cluster is coupled but nodes have not yet recoupled: clusterState=$clusterState")
                  Observable.empty  // Ignore
                } else {
                  logger.warn(s"No heartbeat from the currently active cluster node '$activeId' - trying to fail-over")
                  Observable.fromTask(
                    if (isReplicatingHeadOfFile) {
                      val recoveredJournalFile = continuation.maybeRecoveredJournalFile.getOrElse(
                        throw new IllegalStateException("Failover but nothing has been replicated"))
                      val failedOverStamped = toStampedFailedOver(clusterState,
                        JournalPosition(recoveredJournalFile.fileEventId, lastProperEventPosition))
                      val failedOver = failedOverStamped.value.event
                      common.ifClusterWatchAllowsActivation(clusterState, failedOver,
                        Task {
                          val fileSize = {
                            val file = recoveredJournalFile.file
                            assertThat(exists(file))
                            autoClosing(FileChannel.open(file, APPEND)) { out =>
                              if (out.size > lastProperEventPosition) {
                                logger.info(s"Truncating open transaction in journal '${file.getFileName}' at position $lastProperEventPosition")
                                out.truncate(lastProperEventPosition)
                              }
                              out.write(ByteBuffer.wrap((failedOverStamped.asJson.compactPrint + "\n").getBytes(UTF_8)))
                              //out.force(true)  // sync()
                            }
                            size(file)
                          }
                          //eventWatch.onJournalingStarted(???)
                          eventWatch.onFileWrittenAndEventsCommitted(PositionAnd(fileSize, failedOverStamped.eventId), n = 1)
                          eventWatch.onJournalingEnded(fileSize)
                          builder.startWithState(JournalProgress.InCommittedEventsSection,
                            journalHeader = Some(recoveredJournalFile.journalHeader),
                            eventId = failedOverStamped.eventId,
                            totalEventCount = recoveredJournalFile.calculatedJournalHeader.totalEventCount + 1,
                            recoveredJournalFile.state.applyEvent(failedOver).orThrow)
                          replicatedFirstEventPosition := recoveredJournalFile.firstEventPosition
                          replicatedFileLength = fileSize
                          lastProperEventPosition = fileSize
                          Right(true)
                        })
                    } else {
                      val failedOverStamped = toStampedFailedOver(clusterState,
                        JournalPosition(continuation.fileEventId, lastProperEventPosition))
                      val failedOver = failedOverStamped.value.event
                      common.ifClusterWatchAllowsActivation(clusterState, failedOver,
                        Task {
                          val failedOverJson = failedOverStamped.asJson
                          builder.rollbackToEventSection()
                          builder.put(failedOverJson)
                          if (out.size > lastProperEventPosition) {
                            logger.info(s"Truncating open transaction in journal '${file.getFileName}' at position $lastProperEventPosition")
                            out.truncate(lastProperEventPosition)
                          }
                          out.write(ByteBuffer.wrap((failedOverJson.compactPrint + "\n").getBytes(UTF_8)))
                          //out.force(true)  // sync
                          val fileSize = out.size
                          replicatedFileLength = fileSize
                          lastProperEventPosition = fileSize
                          eventWatch.onFileWrittenAndEventsCommitted(PositionAnd(fileSize, failedOverStamped.eventId), n = 1)
                          Right(true)
                        })
                    }
                  ).flatMap {
                    case Left(problem) => Observable.raiseError(problem.throwable)
                    case Right(false) => Observable.empty   // Ignore
                    case Right(true) => Observable.pure(Right(()))  // End observation
                  }
                }

              case clusterState =>
                logger.trace(s"Ignoring observed pause without heartbeat because cluster is not coupled: clusterState=$clusterState")
                Observable.empty  // Ignore
            }

          case Some((_, JournalSeparators.HeartbeatMarker, _)) =>
            logger.trace(JournalSeparators.HeartbeatMarker.utf8String.trim)
            Observable.empty

          case Some((fileLength, JournalSeparators.EndOfJournalFileMarker, _)) =>
            logger.debug(s"End of replicated journal file reached: ${file.getFileName} eventId=${builder.eventId} fileLength=$fileLength")
            _eof = true
            Observable.pure(Left(EndOfJournalFileMarker))

          case Some((fileLength, line, json)) =>
            out.write(line.toByteBuffer)
            logger.trace(s"Replicated ${continuation.fileEventId}:$fileLength ${line.utf8StringTruncateAt(200).trim}")
            builder.put(json)  // throws on invalid event
            if (isReplicatingHeadOfFile && json.isOfType[JournalEvent, SnapshotTaken.type]) {
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
                out = FileChannel.open(file, APPEND)
                logger.info(s"Continue replicating events into next journal file ${file.getName()}")
                eventWatch.onJournalingStarted(file, journalId,
                  tornLengthAndEventId = PositionAnd(replicatedFileLength/*After EventHeader, before SnapshotTaken, */, continuation.fileEventId),
                  flushedLengthAndEventId = PositionAnd(fileLength, builder.eventId))
                  // ??? Unfortunately not comparable: ensureEqualState(continuation, builder.state)
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
                for (eventId <- json.asObject.flatMap(_("eventId").flatMap(_.asNumber).flatMap(_.toLong))) {
                  eventWatch.onEventsCommitted(PositionAnd(fileLength, eventId), 1)
                }
              }
              if (json == JournalSeparators.Commit) {
                eventWatch.onEventsCommitted(PositionAnd(fileLength, builder.eventId), 1)
                Observable.pure(Right(()))
              } else if (json.toClass[ClusterEvent].isDefined)
                json.as[ClusterEvent].orThrow match {
                  case _: ClusterNodesAppointed | _: ClusterPassiveLost =>
                    Observable.fromTask(
                      sendClusterPrepareCoupling
                        .map(Right.apply))  // TODO Handle heartbeat timeout !

                  case _: ClusterFailedOver =>
                    // Now, this node has switched from still-active (but failed for the other node) to passive.
                    // It's time to recouple.
                    // ClusterPrepareCoupling command requests an event acknowledgement.
                    // To avoid a deadlock, we send ClusterPrepareCoupling command asynchronously and
                    // continue immediately with acknowledgement of ClusterEvent.Coupled.
                    if (!otherFailed)
                      logger.error(s"Replicated unexpected FailedOver event")  // Should not happen
                    dontActivateBecauseOtherFailedOver = false
                    Observable.fromTask(
                      sendClusterPrepareCoupling
                        .map(Right.apply))  // TODO Handle heartbeat timeout !

                  case _: ClusterSwitchedOver =>
                    // Notify ClusterWatch before starting heartbeating
                    val switchedOver = cast[ClusterSwitchedOver](json.as[ClusterEvent].orThrow)
                    Observable.fromTask(
                      common.clusterWatch.applyEvents(from = ownId, switchedOver :: Nil, builder.clusterState, force = true)
                        .map(_.map(_ => Unit)))
                    // TODO sendClusterPassiveFollows ?

                  case ClusterCouplingPrepared(activeId) =>
                    assertThat(activeId != ownId)
                    Observable.fromTask(
                      sendClusterCouple
                        .map(Right.apply))  // TODO Handle heartbeat timeout !

                  case ClusterCoupled(activeId) =>
                    assertThat(activeId != ownId)
                    awaitingCoupledEvent = false
                    Observable.pure(Right(()))

                  case _ =>
                    Observable.pure(Right(()))
                }
              else
                Observable.pure(Right(()))
            }
        }
        .takeWhile(_.left.forall(_ != EndOfJournalFileMarker))
        .takeWhileInclusive(_ => !shouldActivate(builder.clusterState))
        .collect {
          case Left(problem) => problem
          //case Right(Completed) => -ignore-
        }
        .headOptionL
        .map {
          case Some(problem) => Left(problem)
          case None =>
            logger.debug(s"replicateJournalFile(${file.getFileName}) finished, isReplicatingHeadOfFile=$isReplicatingHeadOfFile, " +
              s"replicatedFileLength=$replicatedFileLength, clusterState=${builder.clusterState}")
            if (!isReplicatingHeadOfFile) {
              // In case of fail-over while the next journal file's snapshot is written,
              // we need to remove the next file and cut this file's open transaction
              // So we cut off open transaction already now
              // But this leads to inexactly replicated files !!!
              out.truncate(lastProperEventPosition)
              eventWatch.onJournalingEnded(lastProperEventPosition)
            }
            (builder.fileJournalHeader, builder.calculatedJournalHeader) match {
              case (Some(header), Some(calculatedHeader)) =>
                Right(NextFile(
                  RecoveredJournalFile(file, length = replicatedFileLength, lastProperEventPosition = lastProperEventPosition,
                    header, calculatedHeader, replicatedFirstEventPosition.orThrow, builder.state),
                  builder.clusterState))
              case _ =>
                Left(Problem.pure(s"JournalHeader could not be replicated fileEventId=${continuation.fileEventId} eventId=${builder.eventId}"))
            }
        }
        .guarantee(
          Task { out.close() } >>
            recouplingStreamReader.terminate.map(_ => ()))
    }

  private def testHeartbeatSuppressor(tuple: (Long, ByteVector, Json)): Boolean =
    tuple match {
      case (_, JournalSeparators.HeartbeatMarker, _)
        if clusterConf.testHeartbeatLossPropertyKey.fold(false)(k => sys.props(k).toBoolean) =>
        logger.warn("TEST: Ignoring heartbeat")
        false
      case _ => true
    }

  private def shouldActivate(clusterState: ClusterState) =
    !dontActivateBecauseOtherFailedOver && clusterState.isNonEmptyActive(ownId)

  private def ensureEqualState(continuation: Continuation.Replicatable, snapshot: S)(implicit s: Scheduler): Unit =
    for (recoveredJournalFile <- continuation.maybeRecoveredJournalFile if recoveredJournalFile.state != snapshot) {
      val msg = s"State from recovered journal file ${recoveredJournalFile.fileEventId} does not match snapshot in next journal file"
      logger.error(msg)
      logger.info("Recovered state:")
      try {
        for (snapshotObject <- recoveredJournalFile.state.toSnapshotObservable.toListL.runSyncUnsafe(30.s))
          logger.info("  " + snapshotObject)
        logger.info("Replicated snapshot state:")
        for (snapshotObject <- snapshot.toSnapshotObservable.toListL.runSyncUnsafe(30.s))
          logger.info("  " + snapshotObject)
      } catch { case t: TimeoutException => logger.error(t.toStringWithCauses) }
      sys.error(msg)
    }

  private def toStampedFailedOver(clusterState: Coupled, failedAt: JournalPosition): Stamped[KeyedEvent[ClusterEvent]] = {
    val failedOver = ClusterEvent.ClusterFailedOver(failedActiveId = clusterState.activeId, activatedId = clusterState.passiveId, failedAt)
    val stamped = eventIdGenerator.stamp(NoKey <-: (failedOver: ClusterEvent))
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
      def maybeRecoveredJournalFile: Option[RecoveredJournalFile[S, Event]]
      final lazy val file = journalMeta.file(fileEventId)
    }

    private[PassiveClusterNode] sealed trait HasRecoveredJournalFile
    extends Continuation.Replicatable {
      def recoveredJournalFile: RecoveredJournalFile[S, Event]
      final def maybeJournalId = Some(recoveredJournalFile.journalId)
      final def maybeRecoveredJournalFile = Some(recoveredJournalFile)
    }
  }

  private sealed case class NoLocalJournal(clusterState: ClusterState)
  extends Continuation.Replicatable {
    def fileLength = 0
    def fileEventId = EventId.BeforeFirst
    def firstEventPosition = None
    def lastProperEventPosition = -1L  // Invalid value
    def maybeJournalId = None
    def maybeRecoveredJournalFile = None
  }

  private sealed case class FirstPartialFile(recoveredJournalFile: RecoveredJournalFile[S, Event], clusterState: ClusterState)
  extends Continuation.Replicatable with Continuation.HasRecoveredJournalFile {
    assertThat(recoveredJournalFile.file == file)
    def fileLength = recoveredJournalFile.length
    def fileEventId = recoveredJournalFile.fileEventId
    def firstEventPosition = Some(recoveredJournalFile.firstEventPosition)
    def lastProperEventPosition = recoveredJournalFile.lastProperEventPosition
    override def toString = s"FirstPartialFile($fileEventId,$fileLength,${recoveredJournalFile.eventId})"
  }

  private sealed case class NextFile(recoveredJournalFile: RecoveredJournalFile[S, Event], clusterState: ClusterState)
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
  private final class ServerTimeoutException extends TimeoutException("Journal web service timed out")

  case object ClusterWatchAgreesToActivation
  case object ClusterWatchDisagreeToActivation
}
