package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Path
import java.time.Duration
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class JournalEventReader[E <: Event](
  journalMeta: JournalMeta[E],
  private[journal] val journalFile: Path,
  protected val timeoutLimit: Duration)
  (implicit
    protected val executionContext: ExecutionContext,
    protected val timerService: TimerService)
extends EventReader[E]
{
  import journalMeta.eventJsonCodec

  private val _journalStarted = Promise[Completed]()
  private val eventIdToPositionIndex = new EventIdPositionIndex(size = 1000)
  private var _oldestEventId = EventId(-1)
  private var endPosition = -1L

  protected def started = _journalStarted.future

  private[journal] def onJournalingStarted(positionAndEventId: PositionAnd[EventId]): Unit = {
    endPosition = positionAndEventId.position
    _oldestEventId = positionAndEventId.value
    eventIdToPositionIndex.addAfter(eventId = positionAndEventId.value, position = positionAndEventId.position)
    _journalStarted.success(Completed)
  }

  private[journal] def onEventAdded(flushedPositionAndEventId: PositionAnd[EventId]): Unit = {
    requireJournalingStarted("onEventAdded")
    val PositionAnd(flushedPosition, eventId) = flushedPositionAndEventId
    if (flushedPosition < endPosition)
      throw new IllegalArgumentException(s"JournalEventReader: Added file position $flushedPosition ${EventId.toString(eventId)} < endPosition $endPosition")
    eventIdToPositionIndex.addAfter(eventId = flushedPositionAndEventId.value, position = flushedPositionAndEventId.position)
    endPosition = flushedPosition
    super.onEventAdded(eventId)
  }

  protected def unsafeOldestEventId = {
    requireJournalingStarted("unsafeOldestEventId")
    _oldestEventId
  }

  /**
    * @return `Future(None)` if `after` < `unsafeOldestEventId`
    *         `Future(Some(Iterator.empty))` if no events are available for now
    */
  def eventsAfter(after: EventId): Future[Option[Iterator[Stamped[KeyedEvent[E]]]]] =
    for (_ ← started) yield
      (after >= _oldestEventId) ? {
        val position = eventIdToPositionIndex.positionAfter(after)
        if (position >= endPosition)  // Data behind endPosition is not flushed and probably incomplete
          Iterator.empty
        else
          autoClosing(InputStreamJsonSeqReader.open(journalFile)) { jsonFileReader ⇒
            jsonFileReader.seek(position)
            new Iterator[Stamped[KeyedEvent[E]]] {
              def hasNext = jsonFileReader.position != endPosition
              def next() = jsonFileReader.read().map(_.value)
                .getOrElse(sys.error(s"Unexpected end of journal at position ${jsonFileReader.position}"))
                .as[Stamped[KeyedEvent[E]]].orThrow
            }
            .dropWhile(_.eventId <= after)
            .take(1000/*To prevent OutOfMemoryError until streaming is implemented*/).toVector.toIterator
          }
      }

  protected def reverseEventsAfter(after: EventId) =
    Future.successful(Iterator.empty)  // Not implemented

  private def requireJournalingStarted(method: String): Unit =
    if (!_journalStarted.isCompleted) throw new IllegalStateException(s"JournalFileReader: $method before onJournalStarted?")
}
