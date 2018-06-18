package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.data.event.{Event, EventId}
import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Promise}

/**
  * @author Joacim Zschimmer
  */
final class JournalEventReaderProvider[E <: Event](
  journalMeta: JournalMeta[E],
  private[journal] val journalFile: Path)
  (implicit
    protected val executionContext: ExecutionContext,
    protected val timerService: TimerService)
extends EventReaderProvider[E]
{
  private val eventReaderPromise = Promise[JournalEventReader[E]]()

  val whenRealEventReader = eventReaderPromise.future

  private[journal] def onJournalingStarted(positionAndEventId: PositionAnd[EventId]): JournalEventReader[E] = {
    val eventReader = new JournalEventReader[E](journalMeta, journalFile, positionAndEventId)
    eventReaderPromise.success(eventReader)
    eventReader
  }
}
