package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.event.RealEventReader
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Path
import monix.eval.Task
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class JournalEventReader[E <: Event](
  journalMeta: JournalMeta[E],
  private[journal] val journalFile: Path,
  startPositionAndEventId: PositionAnd[EventId],
  protected val timeoutLimit: FiniteDuration)
  (implicit
    protected val executionContext: ExecutionContext,
    protected val timerService: TimerService)
extends RealEventReader[E]
{
  import journalMeta.eventJsonCodec

  private val eventIdToPositionIndex = new EventIdPositionIndex(size = 1000)
  private var _oldestEventId = EventId(-1)
  private var endPosition = -1L

  endPosition = startPositionAndEventId.position
  _oldestEventId = startPositionAndEventId.value
  eventIdToPositionIndex.addAfter(eventId = startPositionAndEventId.value, position = startPositionAndEventId.position)

  private[journal] def onEventAdded(flushedPositionAndEventId: PositionAnd[EventId]): Unit = {
    val PositionAnd(flushedPosition, eventId) = flushedPositionAndEventId
    if (flushedPosition < endPosition)
      throw new IllegalArgumentException(s"JournalEventReader: Added file position $flushedPosition ${EventId.toString(eventId)} < endPosition $endPosition")
    eventIdToPositionIndex.addAfter(eventId = flushedPositionAndEventId.value, position = flushedPositionAndEventId.position)
    endPosition = flushedPosition
    super.onEventAdded(eventId)
  }

  protected def oldestEventId = _oldestEventId

  /**
    * @return `Task(None)` if `after` < `oldestEventId`
    *         `Task(Some(Iterator.empty))` if no events are available for now
    */
  def eventsAfter(after: EventId): Task[Option[CloseableIterator[Stamped[KeyedEvent[E]]]]] =
    Task.pure(
      (after >= oldestEventId) ? {
        val position = eventIdToPositionIndex.positionAfter(after)
        if (position >= endPosition)  // Data behind endPosition is not flushed and probably incomplete
          CloseableIterator.empty
        else {
          val jsonFileReader = InputStreamJsonSeqReader.open(journalFile)
          closeOnError(jsonFileReader) {
            jsonFileReader.seek(position)
            new CloseableIterator[Stamped[KeyedEvent[E]]] {
              def close() = jsonFileReader.close()
              def hasNext = jsonFileReader.position != endPosition
              def next() = jsonFileReader.read().map(_.value)
                .getOrElse(sys.error(s"Unexpected end of journal at position ${jsonFileReader.position}"))
                .as[Stamped[KeyedEvent[E]]].orThrow
            }
          }
        }
        .dropWhile(_.eventId <= after)
      })

  protected def reverseEventsAfter(after: EventId) =
    Task.pure(CloseableIterator.empty)  // Not implemented
}
