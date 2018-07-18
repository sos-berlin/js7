package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.AbstractJournalEventReader._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[journal] trait AbstractJournalEventReader[E <: Event]
{
  protected val journalMeta: JournalMeta[E]
  protected def journalFile: Path
  protected def tornEventId: EventId
  protected def tornPosition: Long
  protected def endPosition: Long

  import journalMeta.eventJsonCodec

  protected val eventIdToPositionIndex = new EventIdPositionIndex(size = 1000)

  eventIdToPositionIndex.addAfter(tornEventId, tornPosition)

  protected def untornEventsAfter(after: EventId): CloseableIterator[Stamped[KeyedEvent[E]]] = {
    val position = eventIdToPositionIndex.positionAfter(after)
    if (position >= endPosition)  // Data behind endPosition is not flushed and probably incomplete
      CloseableIterator.empty
    else {
      val jsonFileReader = InputStreamJsonSeqReader.open(journalFile)  // Exception when file has been deleted
      val iterator = closeOnError(jsonFileReader) {
        logger.trace(s"'$journalFile' opened. Seek $position after=${EventId.toString(after)}")
        jsonFileReader.seek(position)
        new CloseableIterator[Stamped[KeyedEvent[E]]] {
          def close() = {
            jsonFileReader.close()
            logger.trace(s"'$journalFile' closed")
          }

          def hasNext = jsonFileReader.position != endPosition

          def next() = {
            val stamped = jsonFileReader.read().map(_.value)
              .getOrElse(sys.error(s"Unexpected end of journal file '$journalFile' at position ${jsonFileReader.position}, after=${EventId.toString(after)}"))
              .as[Stamped[KeyedEvent[E]]]
              .orThrow
            eventIdToPositionIndex.tryAddAfter(stamped.eventId, jsonFileReader.position)  // For HistoricJournalEventReader
            stamped
          }

          override def toString = s"CloseableIterator(after = ${EventId.toString(after)})"
        }.closeAtEnd
      }
      var dropped = 0
      iterator.dropWhile { stamped ⇒
        val drop = stamped.eventId <= after
        if (drop) dropped += 1 else if (dropped > 0) { logger.trace(s"'$journalFile' $dropped events dropped"); dropped = 0 }
        drop
      }
    }
  }
}

object AbstractJournalEventReader {
  private val logger = Logger(getClass)
}
