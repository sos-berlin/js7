package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalReader
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[watch] class FileEventIterator[E <: Event](journalMeta: JournalMeta[E], journalFile: Path, tornEventId: EventId, flushedLength: () ⇒ Long)
extends CloseableIterator[Stamped[KeyedEvent[E]]]
{
  private val logger = Logger.withPrefix[FileEventIterator[E]](journalFile.getFileName.toString)
  private val journalReader = new JournalReader(journalMeta, journalFile)
  private var nextEvent: Stamped[KeyedEvent[E]] = null
  private var closed = false

  closeOnError(journalReader) {
    if (journalReader.tornEventId != tornEventId) sys.error(s"Journal file '$journalFile' has different eventId=${journalReader.tornEventId} than expected=$tornEventId")
  }

  def close(): Unit =
    if (!closed) {
      closed = true
      journalReader.close()
    }

  final def firstEventPosition = journalReader.firstEventPosition

  final def seek(positionAndEventId: PositionAnd[EventId]): Unit = {
    journalReader.seekEvent(positionAndEventId)
    nextEvent = null
  }

  /**
    * @return false iff `after` is unknown
    */
  final def skipToEventAfter(after: EventId): Boolean =
    (after >= eventId) && {
      var skipped = 0
      while (eventId < after) {
        if (!hasNext) return false
        next()
        skipped += 1
      }
      if (skipped > 0) logger.trace(s"$skipped events skipped after=$eventId")
      eventId == after
    }

  final def hasNext = nextEvent != null || {
    nextEvent = journalReader.nextEvent().orNull
    nextEvent != null
  }

  final def next(): Stamped[KeyedEvent[E]] =
    if (nextEvent != null) {
      val r = nextEvent
      nextEvent = null
      r
    } else
      journalReader.nextEvent() getOrElse (throw new NoSuchElementException)

  final def eventId = journalReader.eventId
  final def position = journalReader.position
  final def isClosed = closed

  override def toString =
    s"FileEventIterator(${journalFile.getFileName} tornEventId=${EventId.toString(tornEventId)})"
}
