package js7.core.event.journal.recover

import java.nio.file.Path
import js7.base.utils.Assertions.assertThat
import js7.data.event.{JournalHeader, JournalPosition, JournaledState}

final case class RecoveredJournalFile[S <: JournaledState[S]](
  file: Path,
  length: Long,
  /** Last position in events section, but not in a transaction. */
  lastProperEventPosition: Long,
  /** File's JournalHeader. */
  journalHeader: JournalHeader,
  /** The calculated recovered JournalHeader to continue with. */
  calculatedJournalHeader: JournalHeader,
  /** File's position before the events. */
  firstEventPosition: Long,
  /** The recovered state */
  state: S)
{
  assertThat(journalHeader.journalId == calculatedJournalHeader.journalId)
  assertThat(journalHeader.eventId < calculatedJournalHeader.eventId)
  assertThat(journalHeader.totalEventCount < calculatedJournalHeader.totalEventCount)

  def journalId = journalHeader.journalId

  def fileEventId = journalHeader.eventId

  def eventId = calculatedJournalHeader.eventId

  def journalPosition = JournalPosition(fileEventId, length)

  override def toString = s"RecoveredJournalFile($file ($length bytes),$journalHeader,$firstEventPosition,$state,$calculatedJournalHeader)"
}
