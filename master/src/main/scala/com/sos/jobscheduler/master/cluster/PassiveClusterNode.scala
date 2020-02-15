package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import cats.effect.Resource
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
import com.sos.jobscheduler.core.event.journal.recover.{JournalFileStateBuilder, JournalRecovererState, Recovered, RecoveredJournalFile}
import com.sos.jobscheduler.core.event.state.JournalStateBuilder
import com.sos.jobscheduler.data.cluster.ClusterEvent.FailedOver
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.JournalEvent.SnapshotTaken
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, JournalEvent, JournalId, JournalPosition, JournaledState, KeyedEvent, Stamped}
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.ObservablePauseDetector.RichPauseObservable
import com.sos.jobscheduler.master.cluster.PassiveClusterNode._
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterCommand.ClusterPassiveFollows
import io.circe.syntax._
import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{exists, move, newOutputStream, size}
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.StandardOpenOption.{APPEND, CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Path, Paths}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.TimeoutException
import scodec.bits.ByteVector

private[cluster] final class PassiveClusterNode[S <: JournaledState[S, Event]](
  ownUri: Uri,
  activeUri: Uri,
  journalMeta: JournalMeta,
  recovered: Recovered[S, Event],
  clusterConf: ClusterConf,
  eventIdGenerator: EventIdGenerator)
  (implicit actorSystem: ActorSystem)
{
  import recovered.eventWatch

  private val stateBuilderAndAccessor = new StateBuilderAndAccessor[S, Event](recovered.newStateBuilder)
  @volatile
  private var terminated = false

  def state: Task[S] =
    stateBuilderAndAccessor.state

  def run(recoveredClusterState: ClusterState, recoveredState: S): Task[Checked[(ClusterState, ClusterFollowUp[S, Event])]] =
    Task.deferAction { implicit scheduler =>
      assertThat(!terminated)  // Single-use only
      for (o <- recovered.recoveredJournalFile) {
        cutJournalFile(o.file, o.length)
      }
      Task.parMap2(
        clusterPassiveFollows,
        replicateJournalFiles(recoveredClusterState)
      )((_: MasterCommand.Response.Accepted, followUp) => followUp)
        .guarantee(Task {
          terminated = true
        })
    }

  private def cutJournalFile(file: Path, length: Long): Unit =
    if (exists(file) && length < size(file)) {
      autoClosing(new RandomAccessFile(file, "w")) { f =>
        f.setLength(length)
      }
    }

  private def clusterPassiveFollows: Task[MasterCommand.Response.Accepted] =
    masterApi(activeUri, "ClusterPassiveFollows")
      .use(_.executeCommand(
        ClusterPassiveFollows(followedUri = activeUri, followingUri = ownUri)))
      .onErrorRestartLoop(()) { (throwable, _, retry) =>
        logger.warn(s"ClusterPassiveFollows command failed with ${throwable.toStringWithCauses}")
        logger.debug(throwable.toString, throwable)  // A warning should have been issued above
        retry(()).delayExecution(1.s/*TODO*/)
      }

  private def replicateJournalFiles(recoveredClusterState: ClusterState)(implicit s: Scheduler)
  : Task[Checked[(ClusterState, ClusterFollowUp[S, Event])]] =
    masterApi(activeUri, "journal")
      .use(activeMasterApi =>
        Task.tailRecM(
          (recovered.recoveredJournalFile match {
            case None => NoLocalJournal(recoveredClusterState)
            case Some(recoveredJournalFile) => FirstPartialFile(recoveredJournalFile, recoveredClusterState)
          }): Continuation.Replicatable
        )(continuation =>
          replicateJournalFile(continuation, () => stateBuilderAndAccessor.newStateBuilder(), activeMasterApi)
            // TODO Herzschlag auch beim Wechsel zur nächsten Journaldatei prüfen
            .map {
              case Left(problem) => Right(Left(problem))
              case Right(continuation: Continuation.Replicatable) =>
                if (!shouldFinishFollowing(continuation.clusterState))
                  Left(continuation)
                else
                  Right(Right((
                    continuation.clusterState,
                    ClusterFollowUp.BecomeActive(
                      recovered.copy[S, Event](recoveredJournalFile = continuation.maybeRecoveredJournalFile)))))
            }))

  private def masterApi(uri: Uri, name: String): Resource[Task, HttpMasterApi] =
    AkkaHttpMasterApi.resource(baseUri = activeUri, name = name)
      .map[HttpMasterApi](identity)
      .evalTap(_.loginUntilReachable(clusterConf.userAndPassword, Iterator.continually(1.s/*TODO*/)))

  private def replicateJournalFile(
    continuation: Continuation.Replicatable,
    newStateBuilder: () => JournalStateBuilder[S, Event],
    activeMasterApi: HttpMasterApi)
    (implicit scheduler: Scheduler)
  : Task[Checked[Continuation]] =
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
      var inTransaction = false
      var _eof = false
      val builder = new JournalFileStateBuilder[S, Event](journalMeta, journalFileForInfo = file.getFileName,
        continuation.maybeJournalId, newStateBuilder)

      continuation match {
        case FirstPartialFile(recoveredJournalFile, _) =>
          logger.info(s"Start replicating events into journal file ${file.getName()}")
          builder.startWithState(JournalRecovererState.InEventsSection, Some(recoveredJournalFile.journalHeader),
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

        protected def stopRequested = terminated

        override def eof(index: Long) = _eof && index >= replicatedFileLength
      }

      recouplingStreamReader.observe(activeMasterApi, after = continuation.fileLength)
        .doOnError(t => Task {
          logger.debug(s"observeJournalFile($activeMasterApi, fileEventId=${continuation.fileEventId}, " +
            s"position=${continuation.fileLength}) failed with ${t.toStringWithCauses}", t)
        })
        .mapParallelOrdered(sys.runtime.availableProcessors) { case PositionAnd(fileLength, line) =>
          Task((fileLength, line, line.parseJson.orThrow))
        }
        .detectPauses(clusterConf.heartbeat + clusterConf.failAfter)
        .flatMap[Checked[Unit]] {
          case None/*pause*/ =>
            (if (isReplicatingHeadOfFile) continuation.clusterState else builder.clusterState) match {
              case clusterState: ClusterState.Coupled if clusterState.passiveUri == ownUri =>
                // TODO Agenten fragen, ob der andere Knoten noch aktiv ist
                logger.warn(s"Failing over due to missing heartbeat of the currently active cluster node '$activeUri")
                if (isReplicatingHeadOfFile) {
                  val recoveredJournalFile = continuation.maybeRecoveredJournalFile.getOrElse(
                    throw new AssertionError("Failover but nothing has been replicated"))
                  val failedOverStamped = toStampedFailedOver(clusterState, JournalPosition(recoveredJournalFile.fileEventId, recoveredJournalFile.length))
                  val failedOver = failedOverStamped.value.event
                  val fileSize = {
                    val file = recoveredJournalFile.file
                    assertThat(exists(file))
                    autoClosing(newOutputStream(file, APPEND)) { out =>
                      // FIXME EventFailed wird vielleicht nach EventFooter geschrieben
                      out.write((failedOverStamped.asJson.compactPrint + "\n").getBytes(UTF_8))
                      //out.sync()
                    }
                    size(file)
                  }
                  //eventWatch.onJournalingStarted(???)
                  eventWatch.onFileWrittenAndEventsCommitted(PositionAnd(fileSize, failedOverStamped.eventId), n = 1)
                  eventWatch.onJournalingEnded(fileSize)
                  builder.startWithState(JournalRecovererState.InEventsSection,
                    journalHeader = Some(recoveredJournalFile.journalHeader),
                    eventId = failedOverStamped.eventId,
                    totalEventCount = recoveredJournalFile.calculatedJournalHeader.totalEventCount + 1,
                    recoveredJournalFile.state.applyEvent(failedOver).orThrow)
                  replicatedFirstEventPosition := recoveredJournalFile.firstEventPosition
                  replicatedFileLength = fileSize
                  Observable.pure(Right(()))
                } else {
                  val failedOverStamped = toStampedFailedOver(clusterState, JournalPosition(continuation.fileEventId, size(file)))
                  val failedOverJson = failedOverStamped.asJson
                  builder.put(failedOverJson)
                  out.write(ByteBuffer.wrap((failedOverJson.compactPrint + "\n").getBytes(UTF_8)))
                  //out.sync()
                  val fileSize = out.size
                  replicatedFileLength = fileSize
                  eventWatch.onFileWrittenAndEventsCommitted(PositionAnd(fileSize, failedOverStamped.eventId), n = 1)
                  Observable.pure(Right(()))
                }

              case clusterState =>
                logger.trace(s"Ignoring observed pause without heartbeat due to clusterState=$clusterState")
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
            if (!isReplicatingHeadOfFile) {
              eventWatch.onFileWritten(fileLength)
              if (!inTransaction) {
                for (eventId <- json.asObject.flatMap(_("eventId").flatMap(_.asNumber).flatMap(_.toLong))) {
                  eventWatch.onEventsCommitted(PositionAnd(fileLength, eventId), 1)
                }
              }
              if (json == JournalSeparators.Transaction) {
                inTransaction = true
              } else if (inTransaction && json == JournalSeparators.Commit) {
                inTransaction = false
                eventWatch.onEventsCommitted(PositionAnd(fileLength, builder.eventId), 1)
              }
            }
            Observable.pure(Right(()))
        }
        .takeWhile(_.left.forall(_ != EndOfJournalFileMarker))
        .takeWhileInclusive(_ => !shouldFinishFollowing(builder.clusterState))
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
              eventWatch.onJournalingEnded(replicatedFileLength)
            }
            (builder.fileJournalHeader, builder.calculatedJournalHeader) match {
              case (Some(header), Some(calculatedHeader)) =>
                Right(NextFile(
                  RecoveredJournalFile(file, replicatedFileLength, header, calculatedHeader, replicatedFirstEventPosition.orThrow, builder.state),
                  builder.clusterState))
              case _ =>
                Left(Problem.pure(s"JournalHeader could not be replicated fileEventId=${continuation.fileEventId} eventId=${builder.eventId}"))
            }
        }
        .guarantee(
          Task { out.close() } >>
            recouplingStreamReader.terminate.map(_ => ()))
    }

  private def shouldFinishFollowing(clusterState: ClusterState) =
    clusterState.isActive(ownUri)

  private def ensureEqualState(continuation: Continuation.Replicatable, snapshot: S)(implicit s: Scheduler): Unit =
    for (recoveredJournalFile <- continuation.maybeRecoveredJournalFile if recoveredJournalFile.state != snapshot) {
      val msg = s"State from recovered journal file ${ recoveredJournalFile.fileEventId } does not match snapshot in next journal file"
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

  private def toStampedFailedOver(clusterState: ClusterState.Coupled, failedAt: JournalPosition): Stamped[KeyedEvent[ClusterEvent]] = {
    val failedOver = FailedOver(failedActiveUri = clusterState.activeUri, activatedUri = clusterState.passiveUri, failedAt)
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
    def maybeJournalId = None
    def maybeRecoveredJournalFile = None
  }

  private sealed case class FirstPartialFile(recoveredJournalFile: RecoveredJournalFile[S, Event], clusterState: ClusterState)
  extends Continuation.Replicatable with Continuation.HasRecoveredJournalFile {
    assertThat(recoveredJournalFile.file == file)
    def fileLength = recoveredJournalFile.length
    def fileEventId = recoveredJournalFile.fileEventId
    def firstEventPosition = Some(recoveredJournalFile.firstEventPosition)
    override def toString = s"FirstPartialFile($fileEventId,$fileLength,${recoveredJournalFile.eventId})"
  }

  private sealed case class NextFile(recoveredJournalFile: RecoveredJournalFile[S, Event], clusterState: ClusterState)
  extends Continuation.Replicatable with Continuation.HasRecoveredJournalFile {
    /** The next file is initially empty. */
    def fileLength = 0
    /** The next file's EventId is the recovered file's last EventId. */
    def fileEventId = recoveredJournalFile.eventId
    def firstEventPosition = None
    override def toString = s"NextFile(${recoveredJournalFile.eventId})"
  }
}

object PassiveClusterNode
{
  private val TmpSuffix = ".tmp"  // Duplicate in JournalActor
  private val logger = Logger(getClass)

  private val EndOfJournalFileMarker = Problem.pure("End of journal file (internal use only)")
  private final class ServerTimeoutException extends TimeoutException("Journal web service timed out")
}
