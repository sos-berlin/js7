package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.event.{Event, EventId}
import com.typesafe.config.Config
import java.nio.file.{Files, Path}

/**
  * @author Joacim Zschimmer
  */
private[journal] final class HistoricEventReader[E <: Event](
  protected val journalMeta: JournalMeta[E],
  val tornEventId: EventId,
  protected val journalFile: Path,
  protected val config: Config,
  protected val reusableJournalIndex: Option[JournalIndex] = None)
extends AutoCloseable
with EventReader[E]
{
  protected def isHistoric = true
  protected val flushedLength = Files.size(journalFile)

  /** Position of the first event in `journalFile`. */
  protected lazy val tornPosition = iteratorPool.firstEventPosition
}
