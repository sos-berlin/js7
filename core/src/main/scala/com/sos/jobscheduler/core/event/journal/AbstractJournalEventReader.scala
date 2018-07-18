package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.AbstractJournalEventReader._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Path
import monix.execution.atomic.AtomicLong

/**
  * @author Joacim Zschimmer
  */
private[journal] trait AbstractJournalEventReader[E <: Event]
extends AutoCloseable
{
  protected val journalMeta: JournalMeta[E]
  protected def journalFile: Path
  protected def tornEventId: EventId
  protected def tornPosition: Long
  protected def endPosition: Long

  import journalMeta.eventJsonCodec

  protected val eventIdToPositionIndex = new EventIdPositionIndex(size = 1000)
  private val openFilesCount = AtomicLong(0)

  eventIdToPositionIndex.addAfter(tornEventId, tornPosition)

  def close() =
    openFilesCount.get match {
      case n if n > 0 ⇒ logger.debug(s"Closing '$toString' while $n× opened")
      case _ ⇒
    }

  protected def untornEventsAfter(after: EventId): CloseableIterator[Stamped[KeyedEvent[E]]] = {
    val position = eventIdToPositionIndex.positionAfter(after)
    if (position >= endPosition)  // Data behind endPosition is not flushed and probably incomplete
      CloseableIterator.empty
    else {
      val iterator = newCloseableIterator(position = position, after = after).closeAtEnd
      skip(iterator, after = after)
    }
  }

  private def skip(iterator: CloseableIterator[Stamped[KeyedEvent[E]]], after: EventId) = {
    var skipped = 0
    iterator.dropWhile { stamped ⇒
      val drop = stamped.eventId <= after
      if (drop) skipped += 1 else if (skipped > 0) { logger.trace(s"'${journalFile.getFileName}' $skipped events skipped"); skipped = 0 }
      drop
    }
  }

  private def newCloseableIterator(position: Long, after: EventId) = {
    val jsonFileReader = InputStreamJsonSeqReader.open(journalFile)  // Exception when file has been deleted
    openFilesCount.increment()
    closeOnError(jsonFileReader) {
      logger.trace(s"'${journalFile.getFileName}' opened. Seek $position after=${EventId.toString(after)}")
      jsonFileReader.seek(position)
      new EventCloseableIterator(jsonFileReader, after)
    }
  }

  private class EventCloseableIterator(jsonFileReader: InputStreamJsonSeqReader, after: EventId) extends CloseableIterator[Stamped[KeyedEvent[E]]] {
    var closed = false

    def close() =
      if (!closed) {
        jsonFileReader.close()
        openFilesCount.decrement()
        closed = true
        logger.trace(s"'${journalFile.getFileName}' closed")
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

    override def toString = s"CloseableIterator(after = ${EventId.toString(after)}, ${openFilesCount.get}× open)"
  }
}

object AbstractJournalEventReader {
  private val logger = Logger(getClass)
}
