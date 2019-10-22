package com.sos.jobscheduler.master.cluster

import akka.actor.ActorSystem
import cats.effect.Resource
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import com.sos.jobscheduler.common.http.{AkkaHttpClient, RecouplingStreamReader}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.EndOfJournalFileMarker
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, JournalSeparators}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles._
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.state.Recovered
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.JournalEvent.SnapshotTaken
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId}
import com.sos.jobscheduler.master.MasterState
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

final class PassiveClusterNode private(
  myNodeId: ClusterNodeId,
  activeUri: Uri,
  journalMeta: JournalMeta,
  eventWatch: JournalEventWatch,
  recouplingStreamReaderConf: RecouplingStreamReaderConf)
  (implicit actorSystem: ActorSystem)
{
  def run(recovered: Recovered[MasterState, Event], recoveredClusterState: ClusterState): Task[ClusterFollowUp.Terminate.type] =
    Task.deferAction { implicit scheduler =>
      // Cut file after recovered position (garbled record only)
      for (PositionAnd(position, file) <- recovered.positionAndFile) {
        if (Files.exists(file) && position < Files.size(file)) {
          autoClosing(new RandomAccessFile(file, "w")) { f =>
            f.setLength(position)
          }
        }
      }

      // TODO Nach jedem Event (JSON-Feld "eventId" lesen!), dabei Transaktion beachten:
      //  - recovered.eventWatch.onEventsCommitted
      //  - recovered.eventWatch.onFileWritten
      //  - Dabei State aktualisieren
      //  - JournalReader (berücksichtigt Transaktionen)
      //    --> onEventsCommitted, onFileWritten (?)
      //    --> recoverer.updateState
      val recoveredPositionAndFile = recovered.positionAndFile getOrElse PositionAnd(0L, journalMeta.file(EventId.BeforeFirst))
      Resource.fromAutoCloseable(Task {
        AkkaHttpMasterApi(baseUri = activeUri.string)
      }).use(activeNode =>
        Task.parMap2(
          activeNode.executeCommand(MasterCommand.PassiveNodeFollows(myNodeId, activeUri)),   // TODO Retry until success
          replicateJournalFiles(recovered.journalId, recoveredPositionAndFile, recovered.eventId, activeNode, eventWatch)
        )((_: MasterCommand.PassiveNodeFollows.Response, followUp) => followUp))
    }

  /* TODO
      - Direkter Zugriff auf Journaldateien
      - Welche ist die letzte Journaldatei?  --> fileEventId
      - Wenn der letzte Satz in der Journaldatei unvollständig ist (Zeilenende fehlt)
        - Satz abschneiden, damit wir ganze JSON-Werte aus dem Stream lesen und EventId darin erkennen können
      - Länge der Journaldatei --> position
      - Journaldatei direkt beschreiben mit flush (und sync?)
      - Wenn Ende des Streams erreicht ist:
        - EventId des letzten Events ermitteln
          - Das ist der letzte ByteVector, der ein JSON-Objekt ist, also mit '{' beginnt
            - Regel: JSON-Objekt im Journal nach EventHeader ist immer ein Event
            - Letzter Satz kann unvollständig sein (nach Abbruch)
          - Also den merken und am Ende des Streams inspizieren
          - Dann haben wir die nächste fileEventId
   */

  private def replicateJournalFiles(
    recoveredJournalId: Option[JournalId],
    recoveredPositionAndFile: PositionAnd[Path],
    recoveredEventId: EventId,
    masterApi: HttpMasterApi,
    eventWatch: JournalEventWatch)
    (implicit scheduler: Scheduler)
  : Task[ClusterFollowUp.Terminate.type]
  = {
    Task.tailRecM((recoveredJournalId, recoveredEventId, recoveredPositionAndFile)) { case (maybeJournalId, eventId, positionAndFile) =>
      replicateJournalFile(maybeJournalId, positionAndFile, eventId, masterApi, eventWatch)
        .map { case (journalId, eventId) => Left((Some(journalId), eventId, PositionAnd(0, journalMeta.file(eventId)))) }
    }.map(_ => ClusterFollowUp.Terminate)  // TODO tailRecM does not terminate
  }

  private def replicateJournalFile(
    recoveredJournalId: Option[JournalId],
    positionAndFile: PositionAnd[Path],
    eventId: EventId,
    masterApi: HttpMasterApi,
    eventWatch: JournalEventWatch)
    (implicit scheduler: Scheduler)
  : Task[(JournalId, EventId)]
  =
    Task.defer {
      val PositionAnd(position, file) = positionAndFile
      val fileLengthAndEventId = PositionAnd(position, eventId)
      lazy val continueReplicatingMsg = s"Continue replicating events into journal file ${file.getName()}"
      assert(recoveredJournalId.isEmpty == (fileLengthAndEventId == PositionAnd(0L, EventId.BeforeFirst)))
      var isReplicatingSnapshotSection = fileLengthAndEventId.position == 0
      var inTransaction = false
      val maybeTmpFile = isReplicatingSnapshotSection ? Paths.get(file.toString + TmpSuffix)
      maybeTmpFile match {
        case Some(tmpFile) => logger.info(s"Replicating snapshot into journal file ${tmpFile.getName()}")
        case None =>
          logger.info(continueReplicatingMsg)
          eventWatch.onJournalingStarted(file, fileLengthAndEventId, recoveredJournalId.get)
      }
      var replicatedJournalId = SetOnce.fromOption(recoveredJournalId)
      var lastEventId = fileLengthAndEventId.value
      var out = FileChannel.open(maybeTmpFile getOrElse file, CREATE, WRITE, TRUNCATE_EXISTING)
      var eof = false
      var replicatedLength = 0L
      observeJournalFile(masterApi, fileEventId = fileLengthAndEventId.value, position = fileLengthAndEventId.position,
        pos => eof && pos >= replicatedLength
      ).scan((0L, ByteVector.empty))((s, line) => (s._1 + line.length, line))
        .flatMap { case (fileLength, line) =>
          if (line == EndOfJournalFileMarker) {
            eof = true
            Observable.empty
          } else {
            replicatedLength = fileLength
            out.write(line.toByteBuffer)
            val json = line.decodeUtf8.orThrow.parseJsonChecked.orThrow
            for (tmpFile <- maybeTmpFile if isReplicatingSnapshotSection && json.asObject.flatMap(_(TypedJsonCodec.TypeFieldName)).contains(SnapshotTaken.TypeNameJson)) {
              isReplicatingSnapshotSection = false
              out.close()
              move(tmpFile, file, ATOMIC_MOVE)
              out = FileChannel.open(file, APPEND)
              logger.info(continueReplicatingMsg)
              eventWatch.onJournalingStarted(file, PositionAnd(fileLength, fileLengthAndEventId.value), replicatedJournalId())
            }
            if (isReplicatingSnapshotSection) {
              if (replicatedJournalId.isEmpty) {
                // The first line must be a JournalHeader
                replicatedJournalId := json.as[JournalHeader].map(_.journalId).orThrow
              }
            } else {
              eventWatch.onFileWritten(fileLength)
              for (eventId <- json.asObject.flatMap(_("eventId").flatMap(_.asNumber).flatMap(_.toLong))) {
                lastEventId = eventId
                if (!inTransaction) {
                  //logger.debug(s"### eventWatch.onEventsCommitted(PositionAnd($fileLength, $eventId), 1)")
                  eventWatch.onEventsCommitted(PositionAnd(fileLength, eventId), 1)
                }
              }
              if (json == JournalSeparators.Transaction) {
                inTransaction = true
              } else if (inTransaction && json == JournalSeparators.Commit) {
                inTransaction = false
                //logger.debug(s"### eventWatch.onEventsCommitted(PositionAnd($fileLength, $lastEventId), 1)")
                eventWatch.onEventsCommitted(PositionAnd(fileLength, lastEventId), 1)
              }
            }
            Observable.pure(
              (replicatedJournalId.getOrElse(sys.error(s"Missing JournalHeader in replicated journal file '$file'")),
                PositionAnd(fileLength, lastEventId)))
          }
        }
        .guarantee(Task {
          out.close()
        })
        .lastL
        .map { case (journalId, fileLengthAndEventId) =>
          eventWatch.onJournalingEnded(fileLengthAndEventId.position)
          (journalId, fileLengthAndEventId.value)
        }
        // TODO EventId des letzten Events aus Journaldatei lesen (beim Replizieren kann das letzte und einzige Event verstümmelt sein).
        //  Oder einen neuen Webservice befragen?
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
      recouplingStreamReaderConf,
      after = position,
      getObservable = (after: Long) =>
        AkkaHttpClient.liftProblem(
          api.journalObservable(fileEventId = fileEventId, position = after, recouplingStreamReaderConf.timeout, markEOF = true)
            .map(_
              .scan(PositionAnd(after, ByteVector.empty/*unused*/))((s, a) => PositionAnd(s.position + a.length, a)))),
          eof = eof,
    ).map(_.value)  // Return the line without position
}

object PassiveClusterNode
{
  private val TmpSuffix = ".tmp"  // Duplicate in JournalActor
  private val logger = Logger(getClass)

  private[cluster] def run(
    recovered: Recovered[MasterState, Event],
    recoveredClusterState: ClusterState,
    myNodeId: ClusterNodeId,
    activeUri: Uri,
    journalMeta: JournalMeta,
    recouplingStreamReaderConf: RecouplingStreamReaderConf,
    actorSystem: ActorSystem)
    (implicit s: Scheduler)
  : Task[ClusterFollowUp] =
    new PassiveClusterNode(myNodeId, activeUri, journalMeta, recovered.eventWatch, recouplingStreamReaderConf)(actorSystem)
      .run(recovered, recoveredClusterState)

  private final class ServerTimeoutException extends TimeoutException("Journal web service timed out")
}
