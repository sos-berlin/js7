package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import cats.effect.Resource
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.ScodecUtils.RichByteVector
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.http.{AkkaHttpClient, RecouplingStreamReader}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.core.event.journal.data.{JournalMeta, JournalSeparators}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles._
import com.sos.jobscheduler.core.event.journal.recover.{JournalFileStateBuilder, JournalRecovererState, Recovered, RecoveredJournalFile}
import com.sos.jobscheduler.core.event.state.JournalStateBuilder
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.JournalEvent.SnapshotTaken
import com.sos.jobscheduler.data.event.{Event, EventId, JournalEvent, JournalId, JournaledState, KeyedEvent, Stamped}
import com.sos.jobscheduler.master.client.{AkkaHttpMasterApi, HttpMasterApi}
import com.sos.jobscheduler.master.cluster.PassiveClusterNode._
import com.sos.jobscheduler.master.data.MasterCommand
import java.io.RandomAccessFile
import java.nio.channels.FileChannel
import java.nio.file.Files.move
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.StandardOpenOption.{APPEND, CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeoutException
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.{Promise, TimeoutException}
import scodec.bits.ByteVector

final class PassiveClusterNode[S <: JournaledState[S, E], E <: Event] private(
  activeUri: Uri,
  journalMeta: JournalMeta,
  recovered: Recovered[S, E],
  clusterConf: ClusterConf)
  (implicit actorSystem: ActorSystem)
{
  import recovered.eventWatch

  def run(recoveredClusterState: ClusterState, recoveredState: S, getStatePromise: Promise[Task[S]])
  : Task[(ClusterState, ClusterFollowUp)] =
    Task.deferAction { implicit scheduler =>
      for (o <- recovered.recoveredJournalFile) {
        cutJournalFile(o.file, o.length)
      }
      Resource.fromAutoCloseable(Task(AkkaHttpMasterApi(baseUri = activeUri.string)))
        .use(activeNodeApi =>
          activeNodeApi.loginUntilReachable(clusterConf.userAndPassword, Iterator.continually(1.s/*TODO*/)) >>
            Task.parMap2(
              activeNodeApi.executeCommand(MasterCommand.ClusterPassiveFollows(clusterConf.nodeId, activeUri)),   // TODO Retry until success
              replicateJournalFiles(
                continuation = recovered.recoveredJournalFile match {
                  case None => NoLocalJournal(recoveredClusterState)
                  case Some(recoveredJournalFile) => FirstPartialFile(recoveredJournalFile, recoveredClusterState)
                },
                new StateBuilderAndAccessor[S, E](recovered.newStateBuilder, getStatePromise).newStateBuilder _,
                activeNodeApi)
            )((_: MasterCommand.Response.Accepted, followUp) => followUp))
    }

  private def cutJournalFile(file: Path, length: Long): Unit =
    if (Files.exists(file) && length < Files.size(file)) {
      autoClosing(new RandomAccessFile(file, "w")) { f =>
        f.setLength(length)
      }
    }

  private def replicateJournalFiles(
    continuation: Continuation,
    newStateBuilder: () => JournalStateBuilder[S, E],
    masterApi: HttpMasterApi)
    (implicit scheduler: Scheduler)
  : Task[(ClusterState, ClusterFollowUp)]
  =
    Task.tailRecM(continuation)(continuation =>
      replicateJournalFile(continuation, newStateBuilder, masterApi)
        .map(continuation =>
            if (!continuation.clusterState.isActive(clusterConf.nodeId))
              Left(continuation)
            else
              Right((
                continuation.clusterState,
                ClusterFollowUp.BecomeActive(recovered.copy[S, E](
                  recoveredJournalFile = continuation.maybeRecoveredJournalFile))))))

  private def replicateJournalFile(
    continuation: Continuation,
    newStateBuilder: () => JournalStateBuilder[S, E],
    masterApi: HttpMasterApi)
    (implicit scheduler: Scheduler)
  : Task[Continuation] =
    Task.defer {
      import continuation.{file, fileLength => recoveredFileLength}

      val maybeTmpFile = continuation match {
        case _: NoLocalJournal | _: NextFile =>
          val tmp = Paths.get(file.toString + TmpSuffix)
          logger.info(s"Replicating snapshot into journal file ${tmp.getName()}")
          Some(tmp)

        case _: FirstPartialFile =>
          None
      }

      var out = maybeTmpFile match {
        case None => FileChannel.open(file, APPEND)
        case Some(tmp) => FileChannel.open(tmp, CREATE, WRITE, TRUNCATE_EXISTING)
      }
      var isReplicatingHeadOfFile = maybeTmpFile.isDefined
      val replicatedFirstEventPosition = SetOnce.fromOption(continuation.firstEventPosition)
      var replicatedFileLength = recoveredFileLength
      var inTransaction = false
      var eof = false
      val builder = new JournalFileStateBuilder[S, E](journalMeta, journalFileForInfo = file.getFileName,
        continuation.maybeJournalId, newStateBuilder)

      def continueReplicatingMsg = s"Continue replicating events into journal file ${file.getName()}"
      continuation match {
        case FirstPartialFile(recoveredJournalFile, _) =>
          logger.info(continueReplicatingMsg)
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
      observeJournalFile(masterApi, fileEventId = continuation.fileEventId, position = recoveredFileLength,
        eof = pos => eof && pos >= replicatedFileLength
      ) .scan((0L, ByteVector.empty))((s, line) => (s._1 + line.length, line))
        .mapParallelOrdered(sys.runtime.availableProcessors) { case (fileLength, line) =>
          Task((fileLength, line, line.parseJson.orThrow))
        }
        .map {
          case (fileLength, JournalSeparators.EndOfJournalFileMarker, _) =>
            logger.debug(s"End of replicated journal file reached: ${file.getFileName} eventId=${builder.eventId} fileLength=$fileLength")
            eof = true

          case (fileLength, line, json) =>
            builder.put(json)
            out.write(line.toByteBuffer)
            logger.trace(s"Replicated ${line.utf8StringTruncateAt(200).trim}")
            for (tmpFile <- maybeTmpFile if isReplicatingHeadOfFile && json.isOfType[JournalEvent, SnapshotTaken.type]) {
              val journalId = builder.fileJournalHeader.map(_.journalId) getOrElse
                sys.error(s"Missing JournalHeader in replicated journal file '$file'")
              for (o <- continuation.maybeJournalId if o != journalId)
                sys.error(s"Received JournalId '$journalId' does not match expected '$o'")
              replicatedFirstEventPosition := replicatedFileLength
              // SnapshotTaken occurs only as the first event of a journal file, just behind the snapshot
              isReplicatingHeadOfFile = false
              out.close()
              move(tmpFile, file, ATOMIC_MOVE)
              out = FileChannel.open(file, APPEND)
              logger.info(continueReplicatingMsg)
              val stamped = {
                import journalMeta.eventJsonCodec
                json.as[Stamped[KeyedEvent[Event]]].orThrow.asInstanceOf[Stamped[KeyedEvent[SnapshotTaken]]]
              }
              eventWatch.onJournalingStarted(file, journalId,
                tornLengthAndEventId = PositionAnd(replicatedFileLength/*Before SnapshotTaken, after EventHeader*/, continuation.fileEventId),
                flushedLengthAndEventId = PositionAnd(fileLength, stamped.eventId))
                // Unfortunately comparable: ensureEqualState(continuation, builder.state)
            }
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
          ()
        }
        .guarantee(Task { out.close() })
        .takeWhileInclusive(_ => !builder.clusterState.isActive(clusterConf.nodeId))
        .completedL
        .map { _ =>
          logger.debug(s"replicateJournalFile(${file.getFileName}) finished: " + builder.clusterState)
          eventWatch.onJournalingEnded(replicatedFileLength)
          (builder.fileJournalHeader, builder.calculatedJournalHeader) match {
            case (Some(header), Some(calculatedHeader)) =>
              NextFile(
                RecoveredJournalFile(file, replicatedFileLength, header, calculatedHeader, replicatedFirstEventPosition.orThrow,
                  builder.state),
                builder.clusterState)
            case _ =>
              sys.error(s"JournalHeader could not be replicated fileEventId=${continuation.fileEventId} eventId=${builder.eventId}")
          }
        }
    }

  private def observeJournalFile(api: HttpMasterApi, fileEventId: EventId, position: Long,
    eof: Long => Boolean)(implicit s: Scheduler)
  : Observable[ByteVector] =
    RecouplingStreamReader
      .observe[Long/*file position*/, PositionAnd[ByteVector], HttpMasterApi](
        zeroIndex = EventId.BeforeFirst,
        toIndex = _.position,
        api,
        maybeUserAndPassword = clusterConf.userAndPassword,
        clusterConf.recouplingStreamReader,
        after = position,
        getObservable = (after: Long) =>
          AkkaHttpClient.liftProblem(
            api.journalObservable(fileEventId = fileEventId, position = after, clusterConf.recouplingStreamReader.timeout, markEOF = true)
              .map(_
                .scan(PositionAnd(after, ByteVector.empty/*unused*/))((s, a) => PositionAnd(s.position + a.length, a)))),
            eof = eof)
      .map(_.value)  // Return the line without position
      .doOnError(t => Task {
        logger.debug(s"observeJournalFile($api, fileEventId=$fileEventId, position=$position) failed with ${t.toStringWithCauses}", t)
      })

  private def ensureEqualState(continuation: Continuation, snapshot: S)(implicit s: Scheduler): Unit =
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

  private trait Continuation {
    def fileEventId: EventId
    def fileLength: Long
    def clusterState: ClusterState
    def firstEventPosition: Option[Long]
    def maybeJournalId: Option[JournalId]
    def maybeRecoveredJournalFile: Option[RecoveredJournalFile[S, E]]

    final val file = journalMeta.file(fileEventId)
  }

  private final case class NoLocalJournal(clusterState: ClusterState)
  extends Continuation {
    def fileLength = 0
    def fileEventId = EventId.BeforeFirst
    def firstEventPosition = None
    def maybeJournalId = None
    def maybeRecoveredJournalFile = None
  }

  private final case class FirstPartialFile(recoveredJournalFile: RecoveredJournalFile[S, E], clusterState: ClusterState)
  extends Continuation {
    assertThat(file == recoveredJournalFile.file)
    def fileLength = recoveredJournalFile.length
    def fileEventId = recoveredJournalFile.fileEventId
    def firstEventPosition = Some(recoveredJournalFile.firstEventPosition)
    def maybeJournalId = Some(recoveredJournalFile.journalId)
    def maybeRecoveredJournalFile = Some(recoveredJournalFile)
    override def toString = s"FirstPartialFile($fileEventId,$fileLength,${recoveredJournalFile.eventId},…)"
  }

  private final case class NextFile(recoveredJournalFile: RecoveredJournalFile[S, E], clusterState: ClusterState)
  extends Continuation {
    def fileLength = 0
    def fileEventId = recoveredJournalFile.eventId
    def firstEventPosition = None
    def maybeJournalId = Some(recoveredJournalFile.journalId)
    def maybeRecoveredJournalFile = Some(recoveredJournalFile)
    override def toString = s"NextFile(${recoveredJournalFile.eventId},…)"
  }
}

object PassiveClusterNode
{
  private val TmpSuffix = ".tmp"  // Duplicate in JournalActor
  private val logger = Logger(getClass)

  private[cluster] def run[S <: JournaledState[S, E], E <: Event](
    recovered: Recovered[S, E],
    recoveredClusterState: ClusterState,
    recoveredState: S,
    getStatePromise: Promise[Task[S]],
    activeUri: Uri,
    journalMeta: JournalMeta,
    clusterConf: ClusterConf,
    actorSystem: ActorSystem)
    (implicit s: Scheduler)
  : Task[(ClusterState, ClusterFollowUp)] =
    new PassiveClusterNode(activeUri, journalMeta, recovered, clusterConf)(actorSystem)
      .run(recoveredClusterState, recoveredState, getStatePromise)

  private final class ServerTimeoutException extends TimeoutException("Journal web service timed out")
}
