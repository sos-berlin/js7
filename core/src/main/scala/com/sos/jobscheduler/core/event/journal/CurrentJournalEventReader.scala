package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.event.RealEventReader
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import com.sos.jobscheduler.core.event.journal.CurrentJournalEventReader._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Path
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class CurrentJournalEventReader[E <: Event](
  journalMeta: JournalMeta[E],
  journalFile: Path,
  /** Length and after-EventId of initialized and empty journal. */
  flushedLengthAndEventId: PositionAnd[EventId])
  (implicit
    protected val executionContext: ExecutionContext,
    protected val timerService: TimerService)
extends RealEventReader[E]
{
  import journalMeta.eventJsonCodec

  val tornEventId = flushedLengthAndEventId.value
  private var endPosition = flushedLengthAndEventId.position
  private val eventIdToPositionIndex = new EventIdPositionIndex(size = 1000)

  eventIdToPositionIndex.addAfter(eventId = flushedLengthAndEventId.value, position = flushedLengthAndEventId.position)

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
    * @return `Task(None)` if `after` < `tornEventId`
    *         `Task(Some(Iterator.empty))` if no events are available for now
    */
  def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]] =
    (after >= tornEventId) ? {
      val position = eventIdToPositionIndex.positionAfter(after)
      if (position >= endPosition)  // Data behind endPosition is not flushed and probably incomplete
        CloseableIterator.empty
      else {
        val jsonFileReader = InputStreamJsonSeqReader.open(journalFile)  // Exception when file has been deleted since history.fileAfter
        closeOnError(jsonFileReader) {
          jsonFileReader.seek(position)
          new CloseableIterator[Stamped[KeyedEvent[E]]] {
            def close() = jsonFileReader.close()
            def hasNext = jsonFileReader.position != endPosition
            def next() = jsonFileReader.read().map(_.value)
              .getOrElse(sys.error(s"Unexpected end of journal at position ${jsonFileReader.position}"))
              .as[Stamped[KeyedEvent[E]]].orThrow
          }.closeAtEnd
        }
        .dropWhile(_.eventId <= after)
      }
    }

  protected def reverseEventsAfter(after: EventId) =
    CloseableIterator.empty  // Not implemented
}

object CurrentJournalEventReader {
  private val logger = Logger(getClass)
}
