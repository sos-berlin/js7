package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.AutoClosing.closeOnError
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.untilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalReader._
import com.sos.jobscheduler.data.event.JournalSeparators.{Commit, EventHeader, SnapshotFooter, SnapshotHeader, Transaction}
import com.sos.jobscheduler.data.event.{Event, EventId, JournalHeader, JournalId, KeyedEvent, Stamped}
import io.circe.Json
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class JournalReader(journalMeta: JournalMeta, expectedJournalId: Option[JournalId], journalFile: Path)
extends AutoCloseable
{
  private val jsonReader = InputStreamJsonSeqReader.open(journalFile)
  val journalHeader = closeOnError(jsonReader) {
    JournalHeader.checkedHeader(
      jsonReader.read() map (_.value) getOrElse sys.error(s"Journal file '$journalFile' is empty"),
      journalFile, expectedJournalId
    ).orThrow
  }
  logger.debug(journalHeader.toString)

  val tornEventId = journalHeader.eventId
  private var _totalEventCount = journalHeader.totalEventCount
  private var snapshotHeaderRead = false
  private var eventHeaderRead = false
  private var _eventId = tornEventId

  private object transaction
  {
    private var buffer: ArrayBuffer[PositionAnd[Stamped[KeyedEvent[Event]]]] = null
    private var next = 0

    def begin(): Unit = {
      require(!isInTransaction)
      buffer = new mutable.ArrayBuffer[PositionAnd[Stamped[KeyedEvent[Event]]]]
    }

    def add(positionAndStamped: PositionAnd[Stamped[KeyedEvent[Event]]]): Unit = {
      require(isInTransaction)
      buffer += positionAndStamped
    }

    def commit(): Unit = {
      require(isInTransaction)
      next = 0
      if (buffer.isEmpty) buffer = null
    }

    def clear(): Unit =
      buffer = null

    def readNext(): Stamped[KeyedEvent[Event]] = {
      require(isInTransaction)
      val stamped = buffer(next).value
      if (next > 1) buffer(next - 1) = null  // Keep last event for positionAndEventId, free older entry
      next += 1
      if (next == buffer.length) {
        buffer = null
      }
      stamped
    }

    def positionAndEventId: PositionAnd[EventId] = {
      require(isInTransaction)
      // buffer contains >= 2 elements
      PositionAnd(buffer(next).position, buffer(next - 1).value.eventId)
    }

    def isInTransaction = buffer != null

    def length = buffer.length
  }

  def close() = jsonReader.close()

  /** For FileEventIterator */
  lazy val firstEventPosition: Long = {
    if (snapshotHeaderRead || eventHeaderRead) throw new IllegalStateException("JournalReader.firstEventPosition has been called after nextEvent")
    while (nextSnapshotJson().isDefined) {}
    if (!eventHeaderRead) { // No snapshot section
      jsonReader.read() match {
        case Some(PositionAnd(_, EventHeader)) =>
          eventHeaderRead = true
        case None =>
        case Some(positionAndJson) =>
          throw new CorruptJournalException("Event header is missing", journalFile, positionAndJson)
      }
    }
    jsonReader.position
  }

  def nextSnapshots(): Iterator[Any] =
    Iterator(journalHeader) ++ nextSnapshotsWithoutJournalHeader()

  def nextSnapshotsWithoutJournalHeader(): Iterator[Any] =
    untilNoneIterator(nextSnapshotJson()).map(json => journalMeta.snapshotJsonCodec.decodeJson(json).orThrow)

  @tailrec
  private def nextSnapshotJson(): Option[Json] = {
    if (eventHeaderRead) throw new IllegalStateException("nextSnapshotJson has been called after nextEvent")
    val positionAndJson = jsonReader.read() getOrElse sys.error(s"Journal file '$journalFile' is truncated in snapshot section")
    val json = positionAndJson.value
    if (!snapshotHeaderRead)
      json match {
        case EventHeader =>  // Journal file does not have a snapshot section?
          eventHeaderRead = true
          None
        case SnapshotHeader =>
          snapshotHeaderRead = true
          nextSnapshotJson()
        case _ =>
          throw new CorruptJournalException("Snapshot header is missing", journalFile, positionAndJson)
      }
    else if (json.isObject)
      Some(positionAndJson.value)
    else if (json == SnapshotFooter)
      None
    else
      throw new CorruptJournalException("Snapshot footer is missing", journalFile, positionAndJson)
  }

  /** For FileEventIterator */
  def seekEvent(positionAndEventId: PositionAnd[EventId]): Unit = {
    require(positionAndEventId.value >= tornEventId, s"seek($positionAndEventId) but tornEventId=$tornEventId")
    jsonReader.seek(positionAndEventId.position)
    _eventId = positionAndEventId.value
    eventHeaderRead = true
    _totalEventCount = -1
    transaction.clear()
  }

  def nextEvents(): Iterator[Stamped[KeyedEvent[Event]]] =
    untilNoneIterator(nextEvent())

  def nextEvent(): Option[Stamped[KeyedEvent[Event]]] = {
    val result =
      if (transaction.isInTransaction)
        Some(readNextCommitted())
      else
        nextEvent2()
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
          throw new CorruptJournalException(s"Event header is missing", journalFile, positionAndJson)
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
                journalFile, positionAndJson)
            if (_totalEventCount != -1) _totalEventCount += 1
            Some(stampedEvent)

          case Transaction =>
            if (transaction.isInTransaction) throw new CorruptJournalException("Duplicate/nested transaction", journalFile, positionAndJson)
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
                  transaction.commit()
                case Some(o) if o.value.isObject =>
                  transaction.add(o.copy(value = deserialize(o.value)))
                  loop()
              }
            loop()
            if (transaction.isInTransaction) {
              if (_totalEventCount != -1) _totalEventCount += transaction.length
              Some(readNextCommitted())
            } else
              nextEvent3()

          case Commit =>  // Only after seek into a transaction
            nextEvent3()

          case _ => throw new CorruptJournalException(s"Unexpected JSON record", journalFile, positionAndJson)
        }
    }

  private def readNextCommitted(): Stamped[KeyedEvent[Event]] =
    transaction.readNext()

  private def deserialize(json: Json) = {
    import journalMeta.eventJsonCodec
    intelliJuseImport(eventJsonCodec)
    json.as[Stamped[KeyedEvent[Event]]].orThrow
  }

  def eventId = positionAndEventId.value

  def position = positionAndEventId.position

  def positionAndEventId: PositionAnd[EventId] =
    if (transaction.isInTransaction)
      transaction.positionAndEventId
    else
      PositionAnd(jsonReader.position, _eventId)

  def totalEventCount = _totalEventCount
}

private[recover] object JournalReader
{
  private val logger = Logger(getClass)

  private final class CorruptJournalException(message: String, journalFile: Path, positionAndJson: PositionAnd[Json])
  extends RuntimeException(
    s"Journal file '$journalFile' has an error at byte position ${positionAndJson.position}:" +
      s" $message - JSON=${positionAndJson.value.compactPrint.truncateWithEllipsis(50)}")
}
