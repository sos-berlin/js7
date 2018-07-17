package com.sos.jobscheduler.core.event.journal

import com.google.common.annotations.VisibleForTesting
import com.sos.jobscheduler.data.event.{Event, EventId}
import java.nio.file.Path
import monix.execution.atomic.AtomicAny
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
@VisibleForTesting
final case class HistoricJournalFile private[journal](afterEventId: EventId, file: Path)
{
  private val historicJournalEventReader = AtomicAny[HistoricJournalEventReader[_ <: Event]](null)

  def close(): Unit =
    for (r ← Option(historicJournalEventReader.get)) r.close()

  @tailrec
  def eventReader[E <: Event](journalMeta: JournalMeta[E]): HistoricJournalEventReader[E] = {
    historicJournalEventReader.get match {
      case null ⇒
        val r = new HistoricJournalEventReader[E](journalMeta, afterEventId = afterEventId, file)
        if (historicJournalEventReader.compareAndSet(null, r))
          r
        else {
          r.close()
          eventReader(journalMeta)
        }
      case r ⇒ r.asInstanceOf[HistoricJournalEventReader[E]]
    }
  }
}
