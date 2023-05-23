package js7.journal.watch

import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.data.event.{EventId, JournalId}
import js7.journal.data.JournalLocation
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
private[journal] final class HistoricEventReader(
  protected val journalLocation: JournalLocation,
  protected val expectedJournalId: JournalId,
  val fileEventId: EventId,
  protected val journalFile: Path,
  protected val config: Config)
extends AutoCloseable
with EventReader
{
  protected def isHistoric = true

  /** Position of the first event in `journalFile`. */
  protected lazy val firstEventPosition = iteratorPool.firstEventPosition

  protected lazy val committedLength = Files.size(journalFile)

  protected def isFlushedAfterPosition(position: Long) =
    true

  protected def isEOF(position: Long) =
    position >= committedLength

  protected def whenDataAvailableAfterPosition(position: Long, until: MonixDeadline) =
    Task.True/*EOF counts as data*/

  override def toString = s"HistoricEventReader:${journalFile.getFileName}"
}
