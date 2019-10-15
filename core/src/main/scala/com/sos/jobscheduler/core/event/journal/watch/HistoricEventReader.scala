package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.common.scalautil.MonixUtils.ops.RichTaskCompanion
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId}
import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import monix.eval.Task
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
private[journal] final class HistoricEventReader[E <: Event](
  protected val journalMeta: JournalMeta[E],
  protected val expectedJournalId: Option[JournalId],
  val tornEventId: EventId,
  protected val journalFile: Path,
  protected val config: Config)
extends AutoCloseable
with EventReader[E]
{
  protected def isHistoric = true

  /** Position of the first event in `journalFile`. */
  protected lazy val tornPosition = iteratorPool.firstEventPosition

  protected def isFlushedAfterPosition(position: Long) = true

  protected def isEOF(position: Long) = position >= committedLength

  protected lazy val committedLength = Files.size(journalFile)

  protected def whenDataAvailableAfterPosition(position: Long, until: Deadline) = Task.True/*EOF counts as data*/
}
