package js7.journal.recover

import cats.effect.{IO, Resource}
import fs2.Stream
import io.circe.Json
import java.nio.file.Path
import js7.base.ProvisionalAssumptions
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.StreamExtensions.{+:, mapParallelBatch}
import js7.base.problem.Checked.*
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import js7.common.utils.untilNoneIterator
import js7.data.event.JournalSeparators.{Commit, EventHeader, EventHeaderLine, SnapshotFooterLine, SnapshotHeaderLine, Transaction}
import js7.data.event.{Event, EventId, JournalHeader, JournalId, KeyedEvent, SnapshotableState, Stamped}
import js7.journal.recover.JournalReader.*
import scala.annotation.tailrec
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class JournalReader(
  S: SnapshotableState.HasCodec,
  journalFile: Path,
  expectedJournalId: JournalId)
extends AutoCloseable:
  private val jsonReader = InputStreamJsonSeqReader.open(journalFile)

  private val rawJournalHeader: PositionAnd[ByteArray] =
    closeOnError(jsonReader):
      jsonReader.readRaw() getOrElse sys.error(s"Journal file '$journalFile' is empty")

  val journalHeader: JournalHeader =
    val json = jsonReader.toJson(rawJournalHeader).value
    JournalHeader.checkedHeader(json, journalFile, expectedType = S.name, expectedJournalId)
      .orThrow

  val fileEventId: EventId = journalHeader.eventId
  private var _totalEventCount = journalHeader.totalEventCount
  private var snapshotHeaderRead = false
  private var eventHeaderRead = false
  private var _eventId = fileEventId

  private val transaction = new TransactionReader

  def close(): Unit =
    jsonReader.close()

  /** For FileEventIterator, skips snapshot section */
  lazy val firstEventPosition: Long =
    synchronized:
      if snapshotHeaderRead || eventHeaderRead then throw new IllegalStateException(
        "JournalReader.firstEventPosition has been called after nextEvent")
      while nextSnapshotJson().isDefined do {}
      if !eventHeaderRead then // No snapshot section
        jsonReader.read() match
          case Some(PositionAnd(_, EventHeader)) =>
            eventHeaderRead = true
          case None =>
          case Some(positionAndJson) =>
            throw new CorruptJournalException("Event header is missing", journalFile,
              positionAndJson.copy(value = positionAndJson.value.toByteArray))
      jsonReader.position

  private[recover] def readSnapshot: Stream[IO, Any] =
    synchronized:
      journalHeader +:
        Stream.fromIterator[IO](
            untilNoneIterator(nextSnapshotJson()),
            chunkSize = ProvisionalAssumptions.streamChunks.elementsPerChunkLimit)
          .mapParallelBatch()(json => S.snapshotObjectJsonCodec.decodeJson(json).toChecked.orThrow)

  private[recover] def readSnapshotRaw: Stream[IO, ByteArray] =
    synchronized:
      rawJournalHeader.value +:
        Stream.fromIterator[IO](
          untilNoneIterator(nextSnapshotRaw().map(_.value)),
          chunkSize = 1/*???*/)

  private def nextSnapshotJson(): Option[Json] =
    nextSnapshotRaw().map(jsonReader.toJson).map(_.value)

  @tailrec
  private def nextSnapshotRaw(): Option[PositionAnd[ByteArray]] =
    if eventHeaderRead then throw new IllegalStateException("nextSnapshotJson has been called after nextEvent")
    val positionAndRaw = jsonReader.readRaw() getOrElse sys.error(s"Journal file '$journalFile' is truncated in snapshot section")
    val record = positionAndRaw.value
    if !snapshotHeaderRead then
      record match
        case EventHeaderLine =>  // Journal file does not have a snapshot section?
          eventHeaderRead = true
          None
        case SnapshotHeaderLine =>
          snapshotHeaderRead = true
          nextSnapshotRaw()
        case _ =>
          throw new CorruptJournalException("Snapshot header is missing", journalFile, positionAndRaw)
    else if record.headOption contains '{'.toByte/*JSON object?*/ then
      Some(positionAndRaw)
    else if record == SnapshotFooterLine then
      None
    else
      throw new CorruptJournalException("Snapshot footer is missing", journalFile, positionAndRaw)

  /** For FileEventIterator */
  def seekEvent(positionAndEventId: PositionAnd[EventId]): Unit =
    synchronized:
      require(positionAndEventId.value >= fileEventId, s"seek($positionAndEventId) but fileEventId=$fileEventId")
      jsonReader.seek(positionAndEventId.position)
      _eventId = positionAndEventId.value
      eventHeaderRead = true
      _totalEventCount = -1
      transaction.clear()

  private[recover] def readEvents(): Iterator[Stamped[KeyedEvent[Event]]] =
    untilNoneIterator(nextEvent())

  def nextEvent(): Option[Stamped[KeyedEvent[Event]]] =
    synchronized:
      val result = transaction.readNext() orElse nextEvent2()
      for stamped <- result do
        _eventId = stamped.eventId
      result

  private def nextEvent2(): Option[Stamped[KeyedEvent[Event]]] =
    if !eventHeaderRead then
      jsonReader.read() match
        case Some(PositionAnd(_, EventHeader)) =>
          eventHeaderRead = true
          nextEvent3()
        case Some(positionAndJson) =>
          throw new CorruptJournalException("Event header is missing", journalFile,
            positionAndJson.copy(value = positionAndJson.value.toByteArray))

        case None =>
          None
    else
      nextEvent3()

  @tailrec
  private def nextEvent3(): Option[Stamped[KeyedEvent[Event]]] =
    jsonReader.read() match
      case None => None
      case Some(positionAndJson) =>
        positionAndJson.value match
          case json if json.isObject =>
            val stampedEvent = deserialize(positionAndJson.value)
            if stampedEvent.eventId <= _eventId then
              throw new CorruptJournalException(s"Journal is corrupt, EventIds are in wrong order: ${EventId.toString(stampedEvent.eventId)} follows ${EventId.toString(_eventId)}",
                journalFile, positionAndJson.copy(value = positionAndJson.value.toByteArray))

            if _totalEventCount != -1 then _totalEventCount += 1
            Some(stampedEvent)

          case Transaction =>
            if transaction.isInTransaction then throw new CorruptJournalException("Duplicate/nested transaction", journalFile,
              positionAndJson.copy(value = positionAndJson.value.toByteArray))
            transaction.begin()
            def read() =
              try jsonReader.read()
              catch { case NonFatal(t) =>
                transaction.clear()  // Free memory (in case jsonReader has been closed asynchronously)
                throw t
              }
            @tailrec def loop(): Unit =
              read() match
                case None =>
                  // TODO In case a passive cluster node continues reading replicated data after a truncated transaction,
                  //  the transaction buffer should not be cleared.
                  //  Or seek(position before transaction) ?
                  transaction.clear() // File ends before transaction is committed.

                case Some(PositionAnd(_, Commit)) =>
                  transaction.onCommit()

                case Some(o) =>
                  if !o.value.isObject then sys.error(s"Unexpected JSON value in transaction: $o")
                  transaction.add(o.copy(value = deserialize(o.value)))
                  loop()
            loop()
            transaction.readNext() match
              case Some(stamped) =>
                if _totalEventCount != -1 then _totalEventCount += transaction.length
                Some(stamped)
              case None =>
                nextEvent3()

          case Commit =>  // Only after seek into a transaction
            nextEvent3()

          case _ => throw new CorruptJournalException("Unexpected JSON record", journalFile,
            positionAndJson.copy(value = positionAndJson.value.toByteArray))

  private def deserialize(json: Json) =
    import S.keyedEventJsonCodec
    json.as[Stamped[KeyedEvent[Event]]].toChecked.orThrow

  def eventId: EventId =
    positionAndEventId.value

  def position: Long =
    positionAndEventId.position

  def positionAndEventId: PositionAnd[EventId] =
    synchronized:
      transaction.positionAndEventId
        .getOrElse(PositionAnd(jsonReader.position, _eventId))

  def totalEventCount: Long =
    _totalEventCount


object JournalReader:
  def snapshot(S: SnapshotableState.HasCodec, journalFile: Path, expectedJournalId: JournalId)
  : Stream[IO, Any] =
    snapshot_(_.readSnapshot)(S, journalFile, expectedJournalId)

  def rawSnapshot(S: SnapshotableState.HasCodec, journalFile: Path, expectedJournalId: JournalId)
  : Stream[IO, ByteArray] =
    snapshot_(_.readSnapshotRaw)(S, journalFile, expectedJournalId)

  private def snapshot_[A](f: JournalReader => Stream[IO, A])
    (S: SnapshotableState.HasCodec, journalFile: Path, expectedJournalId: JournalId)
  : Stream[IO, A] =
    Stream
      .resource(Resource.fromAutoCloseable(IO(
        new JournalReader(S, journalFile, expectedJournalId))))
      .flatMap(f)

  private final class CorruptJournalException(
    message: String,
    journalFile: Path,
    positionAndJson: PositionAnd[ByteArray])
  extends RuntimeException(
    s"Journal file '$journalFile' has an error at byte position ${positionAndJson.position}:" +
      s" $message - JSON=${positionAndJson.value.utf8String.truncateWithEllipsis(50)}")
