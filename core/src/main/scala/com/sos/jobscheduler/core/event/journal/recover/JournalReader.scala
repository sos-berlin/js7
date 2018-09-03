package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.utils.untilNoneIterator
import com.sos.jobscheduler.core.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import com.sos.jobscheduler.core.event.journal.data.JournalHeaders.{EventFooter, EventHeader, SnapshotFooter, SnapshotHeader}
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta}
import com.sos.jobscheduler.core.event.journal.recover.JournalReader._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import io.circe.Json
import java.nio.file.Path
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
private[journal] final class JournalReader[E <: Event](journalMeta: JournalMeta[E], journalFile: Path) extends AutoCloseable
{
  private val jsonReader = InputStreamJsonSeqReader.open(journalFile)
  val tornEventId = closeOnError(jsonReader) {
    val journalHeader = JournalHeader.checkHeader(jsonReader.read() map (_.value) getOrElse sys.error(s"Journal '$journalFile' is empty"), journalFile)
    journalHeader.eventId
  }
  private var snapshotHeaderRead = false
  private var eventHeaderRead = false
  private var _eventId = tornEventId

  def close() = jsonReader.close()

  /** For FileEventIterator */
  lazy val firstEventPosition: Long = {
    if (snapshotHeaderRead || eventHeaderRead) throw new IllegalStateException("JournalReader.firstEventPosition has been called after nextEvent")
    while (nextSnapshotJson().isDefined) {}
    if (eventHeaderRead)  // No snapshot section
      position
    else
      jsonReader.read() match {
        case Some(PositionAnd(_, EventHeader)) ⇒
          eventHeaderRead = true
          position
        case None ⇒
          position
        case Some(positionAndJson) ⇒
          throw new CorruptJournalException("Event header is missing", journalFile, positionAndJson)
      }
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
  }

  def nextEvents(): Iterator[Stamped[KeyedEvent[E]]] =
    untilNoneIterator(nextEvent())

  @tailrec
  def nextEvent(): Option[Stamped[KeyedEvent[E]]] =
    if (!eventHeaderRead)
      jsonReader.read() match {
        case Some(PositionAnd(_, EventHeader)) ⇒
          eventHeaderRead = true
          nextEvent()
        case Some(positionAndJson) ⇒
          throw new CorruptJournalException(s"Event header is missing", journalFile, positionAndJson)
        case None ⇒
          None
      }
    else
      jsonReader.read() flatMap { positionAndJson ⇒
        val json = positionAndJson.value
        if (json.isObject) {
          import journalMeta.eventJsonCodec
          val stampedEvent = positionAndJson.value.as[Stamped[KeyedEvent[E]]].orThrow
          if (stampedEvent.eventId <= _eventId)
            throw new CorruptJournalException(s"Journal is corrupt, EventIds are out of order: ${EventId.toString(stampedEvent.eventId)} follows ${EventId.toString(_eventId)}",
              journalFile, positionAndJson)
          _eventId = stampedEvent.eventId
          Some(stampedEvent)
        } else {
          if (json != EventFooter) throw new CorruptJournalException("Missing event footer", journalFile, positionAndJson)
          //eventFooterRead = true
          None
        }
      }

  def eventId = _eventId

  def position = jsonReader.position
}

private[recover] object JournalReader {
  private final class CorruptJournalException(message: String, journalFile: Path, positionAndJson: PositionAnd[Json])
  extends RuntimeException(s"Journal file '$journalFile' has an error at byte position ${positionAndJson.position}: $message\n${positionAndJson.value.compactPrint}")
}
