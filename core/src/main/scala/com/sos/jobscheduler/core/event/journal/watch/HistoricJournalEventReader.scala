package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalHeaders.EventsHeader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.event.{Event, EventId}
import java.nio.file.{Files, Path}

/**
  * @author Joacim Zschimmer
  */
private[journal] final class HistoricJournalEventReader[E <: Event](
  protected val journalMeta: JournalMeta[E],
  val tornEventId: EventId,
  protected val journalFile: Path)
extends AutoCloseable
with GenericJournalEventReader[E]
{
  protected val endPosition = Files.size(journalFile)

  /** Position of the first event in `journalFile`. */
  protected lazy val tornPosition = {
    val jsonFileReader = borrowReader()
    try
      Iterator.continually(jsonFileReader.read())
        .collectFirst {
          case Some(PositionAnd(_, EventsHeader)) ⇒ jsonFileReader.position
          case None ⇒ sys.error(s"Invalid journal file '$journalFile', EventHeader is missing")
        }
        .get
    finally
      returnReader(jsonFileReader)
  }

  def eventsAfter(after: EventId) = untornEventsAfter(after)
}
