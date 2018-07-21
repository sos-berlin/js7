package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.core.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import com.sos.jobscheduler.core.event.journal.HistoricJournalEventReader._
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
with AbstractJournalEventReader[E]
{
  protected lazy val tornPosition = firstEventPosition(journalFile)
  protected val endPosition = Files.size(journalFile)

  def eventsAfter(after: EventId) = untornEventsAfter(after)
}

private[journal] object HistoricJournalEventReader
{
  private def firstEventPosition(journalFile: Path) =
    autoClosing(InputStreamJsonSeqReader.open(journalFile)) { jsonFileReader ⇒
      @tailrec def loop(): Long =
        jsonFileReader.read() match {
          case Some(PositionAnd(_, JournalWriter.EventsHeader)) ⇒ jsonFileReader.position
          case Some(_) ⇒ loop()
          case None ⇒ sys.error(s"Invalid journal file '$journalFile', EventHeader is missing")
        }
      loop()
    }
}
