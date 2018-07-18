package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.event.RealEventReader
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.CurrentJournalEventReader._
import com.sos.jobscheduler.data.event.{Event, EventId}
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class CurrentJournalEventReader[E <: Event](
  protected val journalMeta: JournalMeta[E],
  /** Length and after-EventId of initialized and empty journal. */
  flushedLengthAndEventId: PositionAnd[EventId])
  (implicit
    protected val executionContext: ExecutionContext,
    protected val timerService: TimerService)
extends RealEventReader[E]
with AbstractJournalEventReader[E]
{
  val tornEventId = flushedLengthAndEventId.value
  protected val journalFile = journalMeta.file(after = tornEventId)
  protected def tornPosition = flushedLengthAndEventId.position
  protected var endPosition = flushedLengthAndEventId.position  // Initially, the file contains no events

  logger.debug(s"journalFile=$journalFile tornEventId=$tornEventId")

  private[journal] def onEventsAdded(flushedPositionAndEventId: PositionAnd[EventId]): Unit = {
    val PositionAnd(flushedPosition, eventId) = flushedPositionAndEventId
    if (flushedPosition < endPosition)
      throw new IllegalArgumentException(s"CurrentJournalEventReader: Added file position $flushedPosition ${EventId.toString(eventId)} < endPosition $endPosition")
    eventIdToPositionIndex.addAfter(eventId = flushedPositionAndEventId.value, position = flushedPositionAndEventId.position)
    endPosition = flushedPosition
    super.onEventsAdded(eventId)
  }

  /**
    * @return `None` if `after` < `tornEventId`
    *         `Some(Iterator.empty)` if no events are available for now
    */
  def eventsAfter(after: EventId) =
    (after >= tornEventId) ? untornEventsAfter(after)

  protected def reverseEventsAfter(after: EventId) =
    CloseableIterator.empty  // Not implemented
}

object CurrentJournalEventReader {
  private val logger = Logger(getClass)
}
