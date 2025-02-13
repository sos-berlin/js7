package js7.journal.watch

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsEffectExtensions.*
import js7.data.event.{EventId, JournalId}
import js7.journal.data.JournalLocation

/**
  * @author Joacim Zschimmer
  */
private[journal] final class HistoricEventReader(
  protected val journalLocation: JournalLocation,
  protected val expectedJournalId: JournalId,
  val fileEventId: EventId,
  protected val journalFile: Path,
  protected val config: Config,
  protected val ioRuntime: IORuntime)
extends AutoCloseable, EventReader:

  protected def isHistoric = true

  /** Position of the first event in `journalFile`. */
  protected lazy val firstEventPosition = iteratorPool.firstEventPosition

  protected lazy val committedLength = Files.size(journalFile)

  protected def isFlushedAfterPosition(position: Long) =
    true

  protected def isEOF(position: Long) =
    position >= committedLength

  protected def whenDataAvailableAfterPosition(position: Long, until: Option[CatsDeadline]) =
    IO.True/*EOF counts as data*/

  override def toString = s"HistoricEventReader:${journalFile.getFileName}"
