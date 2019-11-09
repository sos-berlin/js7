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
import com.sos.jobscheduler.core.event.journal.recover.{JournalFileStateBuilder, Recovered}
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.JournalEvent.SnapshotTaken
import com.sos.jobscheduler.data.event.{Event, EventId, JournalEvent, JournalId, JournaledState}
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
              replicateJournalFiles(recovered.journalId, recoveredPositionAndFile, recovered.eventId, recoveredState, activeNode, eventWatch)
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
    recoveredEventId: EventId,
    recoveredState: S,
    masterApi: HttpMasterApi,
    eventWatch: JournalEventWatch)
    (implicit scheduler: Scheduler)
  : Task[(ClusterState, ClusterFollowUp)]
  =
    Task.tailRecM((recoveredJournalId, recovered.fileJournalHeader.fold(EventId.BeforeFirst)(_.eventId), recoveredEventId, recoveredPositionAndFile, recoveredState)) {
      case (maybeJournalId, fileEventId, eventId, positionAndFile, state) =>
        replicateJournalFile(maybeJournalId, positionAndFile, fileEventId, eventId, state, masterApi, eventWatch)
          .map {
            case (journalId, eventId, positionAndFile, recoveredJournalHeader, state, clusterState) =>
              if (!clusterState.isActive(clusterConf.nodeId))
                Left((Some(journalId), eventId, eventId, PositionAnd(0, journalMeta.file(eventId)), state))
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

  private def replicateJournalFile(
    recoveredJournalId: Option[JournalId],
    positionAndFile: PositionAnd[Path],
    fileEventId: EventId,
    fromEventId: EventId,
    initialState: S,
    masterApi: HttpMasterApi,
    eventWatch: JournalEventWatch)
    (implicit scheduler: Scheduler)
  : Task[(JournalId, EventId, PositionAnd[Path], Option[JournalHeader], S, ClusterState)]
  =
    Task.defer {
      val PositionAnd(position, file) = positionAndFile
      assertThat(recoveredJournalId.isEmpty == (position == 0 && fromEventId == EventId.BeforeFirst))
      val replicatedJournalId = SetOnce.fromOption(recoveredJournalId)
      val journalFileStateBuilder = new JournalFileStateBuilder[S, E](journalMeta, journalFileForInfo = file.getFileName,
        recoveredJournalId, newStateBuilder)
      var isReplicatingHeadOfFile = position == 0
      var replicatedLength = position
      var lastEventId = fromEventId
      var inTransaction = false
      var eof = false
      lazy val continueReplicatingMsg = s"Continue replicating events into journal file ${file.getName()}"
      val maybeTmpFile = isReplicatingHeadOfFile ? Paths.get(file.toString + TmpSuffix)
      maybeTmpFile match {
        case Some(tmp) => logger.info(s"Replicating snapshot into journal file ${tmp.getName()}")
        case None =>
          logger.info(continueReplicatingMsg)
          eventWatch.onJournalingStarted(file, PositionAnd(position, fromEventId), recoveredJournalId.get)
      }
      var out = maybeTmpFile match {
        case Some(tmp) => FileChannel.open(tmp, CREATE, WRITE, TRUNCATE_EXISTING)
        case None => FileChannel.open(file, APPEND)
      }
      // FIXME Der Aktive hat die neue Journaldatei vielleicht noch nicht angelegt, dann "Unknown journal file=..."
      observeJournalFile(masterApi, fileEventId = fileEventId, position = position, eof = pos => eof && pos >= replicatedLength)
        .scan((0L, ByteVector.empty))((s, line) => (s._1 + line.length, line))
        .mapParallelOrdered(sys.runtime.availableProcessors) { case (fileLength, line) =>
          Task((fileLength, line, line.parseJson.orThrow))
        }.map { case (fileLength, line, json) =>
          if (line == EndOfJournalFileMarker) {
            logger.debug(s"End of replicated journal file reached: ${file.getFileName}")
            eof = true
          } else {
            replicatedLength = fileLength
            journalFileStateBuilder.put(json)
            out.write(line.toByteBuffer)
            logger.trace(s"Replicated ${line.utf8StringTruncateAt(100).trim}")
            for (tmpFile <- maybeTmpFile if isReplicatingHeadOfFile && json.isOfType[JournalEvent, SnapshotTaken.type]) {
              // SnapshotTaken occurs only as the first event of a journal file, just behind the snapshot
              isReplicatingHeadOfFile = false
              out.close()
              move(tmpFile, file, ATOMIC_MOVE)
              out = FileChannel.open(file, APPEND)
              logger.info(continueReplicatingMsg)
              eventWatch.onJournalingStarted(file, PositionAnd(fileLength, fromEventId), replicatedJournalId())
              // TODO journalFileStateBuilder.state mit kommenden Schnappschuss vergleichen. Die müssen gleich sein.
            }
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
          logger.debug(journalFileStateBuilder.clusterState.toString)
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
    RecouplingStreamReader.observe[Long/*file position*/, PositionAnd[ByteVector], HttpMasterApi](
      zeroIndex = EventId.BeforeFirst,
      toIndex = _.position,
      api,
      maybeUserAndPassword = None,
      clusterConf.recouplingStreamReader,
      after = position,
      getObservable = (after: Long) =>
        AkkaHttpClient.liftProblem(
          api.journalObservable(fileEventId = fileEventId, position = after, clusterConf.recouplingStreamReader.timeout, markEOF = true)
            .map(_
              .scan(PositionAnd(after, ByteVector.empty/*unused*/))((s, a) => PositionAnd(s.position + a.length, a)))),
          eof = eof,
    ).map(_.value)  // Return the line without position
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
