package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalHeaders.EventsHeader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.event.{Event, EventId}
import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[journal] final class HistoricEventReader[E <: Event](
  protected val journalMeta: JournalMeta[E],
  val tornEventId: EventId,
  protected val journalFile: Path,
  protected val config: Config)
extends AutoCloseable
with EventReader[E]
{
  protected def isHistoric = true
  protected val endPosition = Files.size(journalFile)

  /** Position of the first event in `journalFile`. */
  protected lazy val tornPosition = {
    val jsonFileReader = borrowReader()  // First call, Should return a new reader
    try
      Iterator.continually(jsonFileReader.read())
        .collectFirst {
          case Some(PositionAnd(_, EventsHeader)) ⇒ jsonFileReader.position
          case None ⇒ sys.error(s"Invalid journal file '$journalFile', EventHeader is missing")
        }
        .get
      catch { case NonFatal(t) ⇒
        jsonFileReader.close()
        throw t
      }
    finally
      returnReader(jsonFileReader)
  }

  def eventsAfter(after: EventId) = untornEventsAfter(after)
}
