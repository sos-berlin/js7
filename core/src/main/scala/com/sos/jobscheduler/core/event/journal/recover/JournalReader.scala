package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.utils.untilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import com.sos.jobscheduler.core.event.journal.data.JournalHeaders.{Commit, EventFooter, EventHeader, SnapshotFooter, SnapshotHeader, Transaction}
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta}
import com.sos.jobscheduler.core.event.journal.recover.JournalReader._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import io.circe.Json
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Joacim Zschimmer
  */
private[journal] final class JournalReader[E <: Event](journalMeta: JournalMeta[E], journalFile: Path) extends AutoCloseable
{
  private val jsonReader = InputStreamJsonSeqReader.open(journalFile)
  val journalHeader = closeOnError(jsonReader) {
    JournalHeader.checkHeader(jsonReader.read() map (_.value) getOrElse sys.error(s"Journal '$journalFile' is empty"), journalFile)
  }
  val tornEventId = journalHeader.eventId
  private var _totalEventCount = journalHeader.totalEventCount
  private var snapshotHeaderRead = false
  private var eventHeaderRead = false
  private var _eventId = tornEventId
  private var transaction: ArrayBuffer[PositionAnd[Stamped[KeyedEvent[E]]]] = null
  private var nextCommitted = 0

  def close() = jsonReader.close()

  /** For FileEventIterator */
  lazy val firstEventPosition: Long = {
    if (snapshotHeaderRead || eventHeaderRead) throw new IllegalStateException("JournalReader.firstEventPosition has been called after nextEvent")
    while (nextSnapshotJson().isDefined) {}
    if (!eventHeaderRead) { // No snapshot section
      jsonReader.read() match {
        case Some(PositionAnd(_, EventHeader)) ⇒
          eventHeaderRead = true
        case None ⇒
        case Some(positionAndJson) ⇒
          throw new CorruptJournalException("Event header is missing", journalFile, positionAndJson)
      }
    }
    jsonReader.position
  }

  def nextSnapshots(): Iterator[Any] =
    untilNoneIterator(nextSnapshotJson()) map (json ⇒ journalMeta.snapshotJsonCodec.decodeJson(json).orThrow)

  @tailrec
  private def nextSnapshotJson(): Option[Json] = {
    if (eventHeaderRead) throw new IllegalStateException("nextSnapshotJson has been called after nextEvent")
    val positionAndJson = jsonReader.read() getOrElse sys.error(s"Journal '$journalFile' is truncated in snapshot section")
    val json = positionAndJson.value
    if (!snapshotHeaderRead)
      json match {
        case EventHeader ⇒  // Journal file has no snapshot section?
          eventHeaderRead = true
          None
        case SnapshotHeader ⇒
          snapshotHeaderRead = true
          nextSnapshotJson()
        case _ ⇒
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
    transaction = null
  }

  def nextEvents(): Iterator[Stamped[KeyedEvent[E]]] =
    untilNoneIterator(nextEvent())

  def nextEvent(): Option[Stamped[KeyedEvent[E]]] =
    if (transaction != null)
      Some(readNextCommitted())
    else {
      val result = nextEvent2()
      for (stamped ← result) {
        _eventId = stamped.eventId
      }
      result
    }

  @tailrec
  private def nextEvent2(): Option[Stamped[KeyedEvent[E]]] =
    if (!eventHeaderRead)
      jsonReader.read() match {
        case Some(PositionAnd(_, EventHeader)) ⇒
          eventHeaderRead = true
          nextEvent2()
        case Some(positionAndJson) ⇒
          throw new CorruptJournalException(s"Event header is missing", journalFile, positionAndJson)
        case None ⇒
          None
      }
    else
      jsonReader.read() match {
        case None ⇒ None
        case Some(positionAndJson) ⇒
          positionAndJson.value match {
            case json if json.isObject ⇒
              val stampedEvent = deserialize(positionAndJson.value)
              if (stampedEvent.eventId <= _eventId)
                throw new CorruptJournalException(s"Journal is corrupt, EventIds are out of order: ${EventId.toString(stampedEvent.eventId)} follows ${EventId.toString(_eventId)}",
                  journalFile, positionAndJson)
              if (_totalEventCount != -1) _totalEventCount += 1
              Some(stampedEvent)

            case Transaction ⇒
              if (transaction != null) throw new CorruptJournalException("Duplicate/nested transaction", journalFile, positionAndJson)
              transaction = new mutable.ArrayBuffer[PositionAnd[Stamped[KeyedEvent[E]]]]
              @tailrec def loop(): Unit =
                jsonReader.read() match {
                  case None ⇒
                    transaction = null  // File ends before transaction is committed.
                  case Some(PositionAnd(_, Commit)) ⇒
                    nextCommitted = 0
                    if (transaction.isEmpty) transaction = null
                  case Some(o) if o.value.isObject ⇒
                    transaction += o.copy(value = deserialize(o.value))
                    loop()
                }
              loop()
              if (transaction == null)
                nextEvent2()
              else {
                if (_totalEventCount != -1) _totalEventCount += transaction.length
                Some(readNextCommitted())
              }

            case Commit ⇒  // Only after seek into a transaction
              nextEvent2()

            case EventFooter ⇒
              None

            case _ ⇒
              throw new CorruptJournalException("Missing event footer", journalFile, positionAndJson)
          }
      }

  private def readNextCommitted(): Stamped[KeyedEvent[E]] = {
    val stamped = transaction(nextCommitted).value
    nextCommitted += 1
    if (nextCommitted == transaction.length) {
      transaction = null
    }
    _eventId = stamped.eventId
    stamped
  }

  private def deserialize(json: Json) = {
    import journalMeta.eventJsonCodec
    json.as[Stamped[KeyedEvent[E]]].orThrow
  }

  def eventId = positionAndEventId.value

  def position = positionAndEventId.position

  def positionAndEventId: PositionAnd[EventId] =
    if (transaction != null)
      PositionAnd(transaction(nextCommitted).position, transaction(nextCommitted - 1).value.eventId)
    else
      PositionAnd(jsonReader.position, _eventId)

  def totalEventCount = _totalEventCount
}

private[recover] object JournalReader {
  private final class CorruptJournalException(message: String, journalFile: Path, positionAndJson: PositionAnd[Json])
  extends RuntimeException(s"Journal file '$journalFile' has an error at byte position ${positionAndJson.position}: $message\n${positionAndJson.value.compactPrint}")
}
