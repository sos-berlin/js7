package js7.journal.recover

import cats.effect.Resource
import io.circe.Json
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked.*
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import js7.common.utils.untilNoneIterator
import js7.data.event.JournalSeparators.{Commit, EventHeader, EventHeaderLine, SnapshotFooterLine, SnapshotHeaderLine, Transaction}
import js7.data.event.{Event, EventId, JournalHeader, JournalId, KeyedEvent, Stamped}
import js7.journal.data.JournalMeta
import js7.journal.recover.JournalReader.*
import monix.eval.Task
import monix.reactive.Observable
import scala.annotation.tailrec
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class JournalReader(journalMeta: JournalMeta, expectedJournalId: JournalId, journalFile: Path)
extends AutoCloseable
{
  private val jsonReader = InputStreamJsonSeqReader.open(journalFile)

  private val rawJournalHeader: PositionAnd[ByteArray] =
    closeOnError(jsonReader) {
      jsonReader.readRaw() getOrElse sys.error(s"Journal file '$journalFile' is empty")
    }

  val journalHeader = {
    val json = jsonReader.toJson(rawJournalHeader).value
    JournalHeader.checkedHeader(json, journalFile, expectedJournalId).orThrow
  }

  val fileEventId = journalHeader.eventId
  private var _totalEventCount = journalHeader.totalEventCount
  private var snapshotHeaderRead = false
  private var eventHeaderRead = false
  private var _eventId = fileEventId

  private val transaction = new TransactionReader

  def close() =
    jsonReader.close()

  /** For FileEventIterator, skips snapshot section */
  lazy val firstEventPosition: Long =
    synchronized {
      if (snapshotHeaderRead || eventHeaderRead) throw new IllegalStateException(
        "JournalReader.firstEventPosition has been called after nextEvent")
      while (nextSnapshotJson().isDefined) {}
      if (!eventHeaderRead) { // No snapshot section
        jsonReader.read() match {
          case Some(PositionAnd(_, EventHeader)) =>
            eventHeaderRead = true
          case None =>
          case Some(positionAndJson) =>
            throw new CorruptJournalException("Event header is missing", journalFile,
              positionAndJson.copy(value = positionAndJson.value.toByteArray))
        }
      }
      jsonReader.position
    }

  private[recover] def readSnapshot: Observable[Any] =
    synchronized {
      journalHeader +:
        Observable.fromIteratorUnsafe(untilNoneIterator(nextSnapshotJson()))
          .mapParallelBatch()(json => journalMeta.snapshotObjectJsonCodec.decodeJson(json).toChecked.orThrow)
    }

  private[recover] def readSnapshotRaw: Observable[ByteArray] =
    synchronized {
      rawJournalHeader.value +:
        Observable.fromIteratorUnsafe(untilNoneIterator(nextSnapshotRaw().map(_.value)))
    }

  private def nextSnapshotJson(): Option[Json] =
    nextSnapshotRaw().map(jsonReader.toJson).map(_.value)

  @tailrec
  private def nextSnapshotRaw(): Option[PositionAnd[ByteArray]] = {
    if (eventHeaderRead) throw new IllegalStateException("nextSnapshotJson has been called after nextEvent")
    val positionAndRaw = jsonReader.readRaw() getOrElse sys.error(s"Journal file '$journalFile' is truncated in snapshot section")
    val record = positionAndRaw.value
    if (!snapshotHeaderRead)
      record match {
        case EventHeaderLine =>  // Journal file does not have a snapshot section?
          eventHeaderRead = true
          None
        case SnapshotHeaderLine =>
          snapshotHeaderRead = true
          nextSnapshotRaw()
        case _ =>
          throw new CorruptJournalException("Snapshot header is missing", journalFile, positionAndRaw)
      }
    else if (record.headOption contains '{'.toByte/*JSON object?*/)
      Some(positionAndRaw)
    else if (record == SnapshotFooterLine)
      None
    else
      throw new CorruptJournalException("Snapshot footer is missing", journalFile, positionAndRaw)
  }

  /** For FileEventIterator */
  def seekEvent(positionAndEventId: PositionAnd[EventId]): Unit =
    synchronized {
      require(positionAndEventId.value >= fileEventId, s"seek($positionAndEventId) but fileEventId=$fileEventId")
      jsonReader.seek(positionAndEventId.position)
      _eventId = positionAndEventId.value
      eventHeaderRead = true
      _totalEventCount = -1
      transaction.clear()
    }

  private[recover] def readEvents(): Iterator[Stamped[KeyedEvent[Event]]] =
    untilNoneIterator(nextEvent())

  def nextEvent(): Option[Stamped[KeyedEvent[Event]]] =
    synchronized {
      val result = transaction.readNext() orElse nextEvent2()
      for (stamped <- result) {
        _eventId = stamped.eventId
      }
      result
    }

  private def nextEvent2(): Option[Stamped[KeyedEvent[Event]]] =
    if (!eventHeaderRead)
      jsonReader.read() match {
        case Some(PositionAnd(_, EventHeader)) =>
          eventHeaderRead = true
          nextEvent3()
        case Some(positionAndJson) =>
          throw new CorruptJournalException("Event header is missing", journalFile,
            positionAndJson.copy(value = positionAndJson.value.toByteArray))

        case None =>
          None
      }
    else
      nextEvent3()

  @tailrec
  private def nextEvent3(): Option[Stamped[KeyedEvent[Event]]] =
    jsonReader.read() match {
      case None => None
      case Some(positionAndJson) =>
        positionAndJson.value match {
          case json if json.isObject =>
            val stampedEvent = deserialize(positionAndJson.value)
            if (stampedEvent.eventId <= _eventId)
              throw new CorruptJournalException(s"Journal is corrupt, EventIds are in wrong order: ${EventId.toString(stampedEvent.eventId)} follows ${EventId.toString(_eventId)}",
                journalFile, positionAndJson.copy(value = positionAndJson.value.toByteArray))

            if (_totalEventCount != -1) _totalEventCount += 1
            Some(stampedEvent)

          case Transaction =>
            if (transaction.isInTransaction) throw new CorruptJournalException("Duplicate/nested transaction", journalFile,
              positionAndJson.copy(value = positionAndJson.value.toByteArray))
            transaction.begin()
            def read() =
              try jsonReader.read()
              catch { case NonFatal(t) =>
                transaction.clear()  // Free memory (in case jsonReader has been closed asynchronously)
                throw t
              }
            @tailrec def loop(): Unit =
              read() match {
                case None =>
                  // TODO In case a passive cluster node continues reading replicated data after a truncated transaction,
                  //  the transaction buffer should not be cleared.
                  //  Or seek(position before transaction) ?
                  transaction.clear() // File ends before transaction is committed.

                case Some(PositionAnd(_, Commit)) =>
                  transaction.onCommit()

                case Some(o) =>
                  if (!o.value.isObject) sys.error(s"Unexpected JSON value in transaction: $o")
                  transaction.add(o.copy(value = deserialize(o.value)))
                  loop()
              }
            loop()
            transaction.readNext() match {
              case Some(stamped) =>
                if (_totalEventCount != -1) _totalEventCount += transaction.length
                Some(stamped)
              case None =>
                nextEvent3()
            }

          case Commit =>  // Only after seek into a transaction
            nextEvent3()

          case _ => throw new CorruptJournalException("Unexpected JSON record", journalFile,
            positionAndJson.copy(value = positionAndJson.value.toByteArray))
        }
    }

  private def deserialize(json: Json) = {
    import journalMeta.implicitEventJsonCodec
    intelliJuseImport(implicitEventJsonCodec)
    json.as[Stamped[KeyedEvent[Event]]].toChecked.orThrow
  }

  def eventId = positionAndEventId.value

  def position = positionAndEventId.position

  def positionAndEventId: PositionAnd[EventId] =
    synchronized {
      transaction.positionAndEventId
        .getOrElse(PositionAnd(jsonReader.position, _eventId))
    }

  def totalEventCount = _totalEventCount
}

object JournalReader
{
  def snapshot(journalMeta: JournalMeta, expectedJournalId: JournalId, journalFile: Path)
  : Observable[Any] =
    snapshot_(_.readSnapshot)(journalMeta, expectedJournalId, journalFile)

  def rawSnapshot(journalMeta: JournalMeta, expectedJournalId: JournalId, journalFile: Path)
  : Observable[ByteArray] =
    snapshot_(_.readSnapshotRaw)(journalMeta, expectedJournalId, journalFile)

  private def snapshot_[A](f: JournalReader => Observable[A])
    (journalMeta: JournalMeta, expectedJournalId: JournalId, journalFile: Path)
  : Observable[A] =
    Observable
      .fromResource(Resource.fromAutoCloseable(Task(
        new JournalReader(journalMeta, expectedJournalId, journalFile))))
      .flatMap(f)

  private final class CorruptJournalException(
    message: String,
    journalFile: Path,
    positionAndJson: PositionAnd[ByteArray])
  extends RuntimeException(
    s"Journal file '$journalFile' has an error at byte position ${positionAndJson.position}:" +
      s" $message - JSON=${positionAndJson.value.utf8String.truncateWithEllipsis(50)}")
}
