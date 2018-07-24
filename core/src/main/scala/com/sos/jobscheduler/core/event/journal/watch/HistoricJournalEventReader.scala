package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalHeaders.EventsHeader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.event.{Event, EventId}
import java.nio.file.{Files, Path}
import scala.annotation.tailrec

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
  protected lazy val tornPosition = firstEventPosition(journalFile)
  protected val endPosition = Files.size(journalFile)

  def eventsAfter(after: EventId) = untornEventsAfter(after)

  private def firstEventPosition(journalFile: Path) = {
    val jsonFileReader = borrowReader()
    try {
      @tailrec def loop(): Long =
        jsonFileReader.read() match {
          case Some(PositionAnd(_, EventsHeader)) ⇒ jsonFileReader.position
          case Some(_) ⇒ loop()
          case None ⇒ sys.error(s"Invalid journal files '$journalFile', EventHeader is missing")
        }
      loop()
    }
    finally returnReader(jsonFileReader)
  }
}
