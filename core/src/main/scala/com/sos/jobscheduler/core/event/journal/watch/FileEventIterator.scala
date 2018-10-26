package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime.RichConcurrentDuration
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalReader
import com.sos.jobscheduler.core.event.journal.watch.FileEventIterator._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Path
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
private[watch] class FileEventIterator[E <: Event](
  journalMeta: JournalMeta[E],
  journalFile: Path,
  tornEventId: EventId,
  flushedLength: () ⇒ Long)
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

  /** Make take minutes for a gigabygte journal..
    * @return false iff `after` is unknown
    */
  final def skipToEventAfter(eventIdPositionIndex: EventIdPositionIndex, after: EventId): Boolean =
    eventId <= after &&
      (eventId == after ||
        eventIdPositionIndex.synchronizeBuilding {  // After timeout a client may try again. We synchronize these probably idempotent calls
          // May take a long time !!!
          val watch = new TimeWatch(after)
          while (eventId < after) {
            if (!hasNext) return false
            next()
            val PositionAnd(position, eventId) = positionAndEventId
            eventIdPositionIndex.tryAddAfter(eventId, position)
            watch.onSkipped()  // Occassion to update EventIdPositionIndex
          }
          watch.end()
          eventId == after
        })

  final def hasNext = nextEvent != null ||
    journalReader.position < flushedLength() && {
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
  final def positionAndEventId = journalReader.positionAndEventId
  final def isClosed = closed

  override def toString =
    s"FileEventIterator(${journalFile.getFileName} tornEventId=${EventId.toString(tornEventId)} eventId=$eventId)"

  private class TimeWatch(after: EventId) {
    private val PositionAnd(startPosition, startEventId) = positionAndEventId
    private val startedAt = now
    private var skipped = 0
    private var debugIssued = false

    def onSkipped(): Unit = {
      skipped += 1
      val duration = now - startedAt
      def msg = s"$skipped events (${toKBGB(position - startPosition)}) skipped since ${duration.pretty}" +
        s" while searching ${EventId.toDateTimeString(startEventId)}..${EventId.toDateTimeString(after)} in journal, "
      if (!debugIssued && (position - startPosition >= 100*1000*1000 || duration > 10.seconds)) {
        logger.debug(msg)
        debugIssued = true
      }
    }

    def end(): Unit = {
      val skippedSize = position - startPosition
      val duration = now - startedAt
      if (skipped > 0)
        logger.trace(s"$skipped events (${toKBGB(skippedSize)}) skipped in ${duration.pretty} for searching ${EventId.toString(startEventId)}..${EventId.toString(after)}")
      if (skippedSize >= WarnSkippedSize || duration >= WarnDuration)
        logger.info(s"$skipped events (${toKBGB(skippedSize)}) read in ${duration.pretty}, in search of event '${EventId.toString(startEventId)}'")
    }
  }
}

object FileEventIterator {
  private val WarnSkippedSize = 100*1000*100
  private val WarnDuration = 30.seconds
}
