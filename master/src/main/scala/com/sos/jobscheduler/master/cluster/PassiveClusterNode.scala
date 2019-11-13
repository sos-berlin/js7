package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import cats.effect.Resource
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.ScodecUtils.RichByteVector
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.http.{AkkaHttpClient, RecouplingStreamReader}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.EndOfJournalFileMarker
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, JournalSeparators}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles._
import com.sos.jobscheduler.core.event.journal.recover.{JournalFileStateBuilder, JournalRecovererState, Recovered}
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
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
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.TimeoutException
import scodec.bits.ByteVector

// TODO Nach core.journal schieben?

final class PassiveClusterNode[S <: JournaledState[S, E], E <: Event] private(
  activeUri: Uri,
  journalMeta: JournalMeta,
  recovered: Recovered[S, E],
  clusterConf: ClusterConf)
  (implicit actorSystem: ActorSystem)
{
  import recovered.{eventWatch, newStateBuilder}

  def run(recoveredState: S, recoveredClusterState: ClusterState): Task[(ClusterState, ClusterFollowUp)] =
    Task.deferAction { implicit scheduler =>
      for (PositionAnd(length, file) <- recovered.positionAndFile) {
        cutJournalFile(file, length)
      }
      val recoveredPositionAndFile = recovered.positionAndFile getOrElse PositionAnd(0L, journalMeta.file(EventId.BeforeFirst))
      Resource.fromAutoCloseable(Task(AkkaHttpMasterApi(baseUri = activeUri.string)))
        .use(activeNode =>
          activeNode.loginUntilReachable(clusterConf.userAndPassword, Iterator.continually(1.s/*TODO*/)) >>
            Task.parMap2(
              activeNode.executeCommand(MasterCommand.PassiveNodeFollows(clusterConf.nodeId, activeUri)),   // TODO Retry until success
              replicateJournalFiles(recovered.journalId, recoveredPositionAndFile,
                recovered.firstEventPosition, recovered.eventId, recoveredState, activeNode, eventWatch)
            )((_: MasterCommand.Response.Accepted, followUp) => followUp))
    }

  private def cutJournalFile(file: Path, length: Long): Unit =
    if (Files.exists(file) && length < Files.size(file)) {
      autoClosing(new RandomAccessFile(file, "w")) { f =>
        f.setLength(length)
      }
    }

  private def replicateJournalFiles(
    recoveredJournalId: Option[JournalId],
    recoveredPositionAndFile: PositionAnd[Path],
    recoveredFirstEventPosition: Option[Long],
    recoveredEventId: EventId,
    recoveredState: S,
    masterApi: HttpMasterApi,
    eventWatch: JournalEventWatch)
    (implicit scheduler: Scheduler)
  : Task[(ClusterState, ClusterFollowUp)]
  = {
    val eventId = recovered.fileJournalHeader.fold(EventId.BeforeFirst)(_.eventId)
    Task.tailRecM((recoveredJournalId, eventId, recoveredFirstEventPosition, recoveredEventId, recoveredPositionAndFile, recoveredState)) {
      case (maybeJournalId, fileEventId, firstEventPosition, eventId, positionAndFile, state) =>
        assertThat(positionAndFile.value == journalMeta.file(fileEventId))
        replicateJournalFile(maybeJournalId, positionAndFile, firstEventPosition, fileEventId, eventId, state, masterApi, eventWatch)
          .map {
            case (journalId, eventId, positionAndFile, recoveredJournalHeader, state, clusterState) =>
              if (!clusterState.isActive(clusterConf.nodeId))
                Left((Some(journalId), eventId, None, eventId, PositionAnd(0, journalMeta.file(eventId)), state))
              else
                Right((
                  clusterState,
                  ClusterFollowUp.BecomeActive(recovered.copy[S, E](
                    eventId = eventId,
                    positionAndFile = Some(positionAndFile),
                    recoveredJournalHeader = recoveredJournalHeader,
                    maybeState = Some(state)))))
          }
    }
  }

  private def replicateJournalFile(
    recoveredJournalId: Option[JournalId],
    positionAndFile: PositionAnd[Path],
    maybeFirstEventPosition: Option[Long],
    fileEventId: EventId,
    fromEventId: EventId,
    initialState: S,
    masterApi: HttpMasterApi,
    eventWatch: JournalEventWatch)
    (implicit scheduler: Scheduler)
  : Task[(JournalId, EventId, PositionAnd[Path], Option[JournalHeader], S, ClusterState)]
  =
    Task.defer {
      val PositionAnd(fromPosition, file) = positionAndFile
      assertThat(recoveredJournalId.isEmpty == (fromPosition == 0 && fromEventId == EventId.BeforeFirst))
      val replicatedJournalId = SetOnce.fromOption(recoveredJournalId)
      var isReplicatingHeadOfFile = fromPosition == 0
      var replicatedLength = fromPosition
      var lastEventId = fromEventId
      var inTransaction = false
      var eof = false
      val journalFileStateBuilder = new JournalFileStateBuilder[S, E](journalMeta, journalFileForInfo = file.getFileName,
        recoveredJournalId, newStateBuilder)
      if (!isReplicatingHeadOfFile) {
        // Must be the first replicated journal file
        assertThat(recovered.fileJournalHeader.map(_.eventId) contains fileEventId)
        journalFileStateBuilder.startWithState(JournalRecovererState.InEventsSection, recovered.fileJournalHeader,
          recovered.maybeState getOrElse sys.error("Missing recovered state for first replicated journal file"))
      }
      lazy val continueReplicatingMsg = s"Continue replicating events into journal file ${file.getName()}"
      val maybeTmpFile = isReplicatingHeadOfFile ? Paths.get(file.toString + TmpSuffix)
      maybeTmpFile match {
        case Some(tmp) => logger.info(s"Replicating snapshot into journal file ${tmp.getName()}")
        case None =>
          logger.info(continueReplicatingMsg)
          val firstEventPosition = maybeFirstEventPosition getOrElse sys.error(s"replicateJournalFile($fileEventId): missing fileEventPosition")
          eventWatch.onJournalingStarted(file,
            recoveredJournalId.get,
            tornLengthAndEventId = PositionAnd(firstEventPosition, fileEventId),
            flushedLengthAndEventId = PositionAnd(fromPosition, fromEventId))
      }
      var out = maybeTmpFile match {
        case Some(tmp) => FileChannel.open(tmp, CREATE, WRITE, TRUNCATE_EXISTING)
        case None => FileChannel.open(file, APPEND)
      }
      // FIXME Der Aktive hat die neue Journaldatei vielleicht noch nicht angelegt, dann "Unknown journal file=..."
      observeJournalFile(masterApi, fileEventId = fileEventId, position = fromPosition, eof = pos => eof && pos >= replicatedLength)
        .scan((0L, ByteVector.empty))((s, line) => (s._1 + line.length, line))
        .mapParallelOrdered(sys.runtime.availableProcessors) { case (fileLength, line) =>
          Task((fileLength, line, line.parseJson.orThrow))
        }.map { case (fileLength, line, json) =>
          if (line == EndOfJournalFileMarker) {
            logger.debug(s"End of replicated journal file reached: ${file.getFileName} eventId=$lastEventId fileLength=$fileLength")
            eof = true
          } else {
            journalFileStateBuilder.put(json)
            out.write(line.toByteBuffer)
            logger.trace(s"Replicated ${line.utf8StringTruncateAt(100).trim}")
            for (tmpFile <- maybeTmpFile if isReplicatingHeadOfFile && json.isOfType[JournalEvent, SnapshotTaken.type]) {
              val stamped = {
                import journalMeta.eventJsonCodec
                json.as[Stamped[KeyedEvent[Event]]].orThrow.asInstanceOf[Stamped[KeyedEvent[SnapshotTaken]]]
              }
              // SnapshotTaken occurs only as the first event of a journal file, just behind the snapshot
              isReplicatingHeadOfFile = false
              out.close()
              move(tmpFile, file, ATOMIC_MOVE)
              out = FileChannel.open(file, APPEND)
              logger.info(continueReplicatingMsg)
              eventWatch.onJournalingStarted(file, replicatedJournalId(),
                tornLengthAndEventId = PositionAnd(replicatedLength/*Before SnapshotTaken, after EventHeader*/, fileEventId),
                flushedLengthAndEventId = PositionAnd(fileLength, stamped.eventId))
              // TODO journalFileStateBuilder.state mit kommenden Schnappschuss vergleichen. Die müssen gleich sein.
            }
            replicatedLength = fileLength
            if (isReplicatingHeadOfFile) {
              if (replicatedJournalId.isEmpty) {
                // The first line must be a JournalHeader
                replicatedJournalId := json.as[JournalHeader].map(_.journalId).orThrow
              }
            } else {
              eventWatch.onFileWritten(fileLength)
              for (eventId <- json.asObject.flatMap(_("eventId").flatMap(_.asNumber).flatMap(_.toLong))) {
                lastEventId = eventId
                if (!inTransaction) {
                  eventWatch.onEventsCommitted(PositionAnd(fileLength, eventId), 1)
                }
              }
              if (json == JournalSeparators.Transaction) {
                inTransaction = true
              } else if (inTransaction && json == JournalSeparators.Commit) {
                inTransaction = false
                eventWatch.onEventsCommitted(PositionAnd(fileLength, lastEventId), 1)
              }
            }
          }
          ()
        }
        .guarantee(Task { out.close() })
        .takeWhileInclusive(_ => !journalFileStateBuilder.clusterState.isActive(clusterConf.nodeId))
        .completedL
        .map { _ =>
          logger.debug(s"replicateJournalFile(${file.getFileName}) finished: " + journalFileStateBuilder.clusterState)
          eventWatch.onJournalingEnded(replicatedLength)
          val journalId = replicatedJournalId getOrElse sys.error(s"Missing JournalHeader in replicated journal file '$file'")
          (journalId, lastEventId, PositionAnd(replicatedLength, file),
            journalFileStateBuilder.recoveredJournalHeader, journalFileStateBuilder.state, journalFileStateBuilder.clusterState)
        }
    }

  private def observeJournalFile(api: HttpMasterApi, fileEventId: EventId, position: Long, eof: Long => Boolean)
    (implicit s: Scheduler): Observable[ByteVector] =
    // TODO MasterState.applyEvent mit empfangenen Events
    //  Dabei ClusterEvents beachten und bei ggfs. aktivieren (oder beenden).
    //  JournalEventWatch onJournalingStarted, onFileWritten, onEventCommitted (ganzes JSON lesen, Transaktion zurückhalten?)
    //  Oder JournalReader, das geht mit Transaktionen um
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
}

object PassiveClusterNode
{
  private val TmpSuffix = ".tmp"  // Duplicate in JournalActor
  private val logger = Logger(getClass)

  private[cluster] def run[S <: JournaledState[S, E], E <: Event](
    recovered: Recovered[S, E],
    recoveredClusterState: ClusterState,
    recoveredState: S,
    activeUri: Uri,
    journalMeta: JournalMeta,
    clusterConf: ClusterConf,
    actorSystem: ActorSystem)
    (implicit s: Scheduler)
  : Task[(ClusterState, ClusterFollowUp)] =
    new PassiveClusterNode(activeUri, journalMeta, recovered, clusterConf)(actorSystem)
      .run(recoveredState, recoveredClusterState)

  private final class ServerTimeoutException extends TimeoutException("Journal web service timed out")
}
