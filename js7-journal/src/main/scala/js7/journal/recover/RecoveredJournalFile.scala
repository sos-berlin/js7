package js7.journal.recover

import java.nio.file.Path
import js7.base.utils.Assertions.assertThat
import js7.data.event.{JournalHeader, JournalPosition, SnapshotableState}

final case class RecoveredJournalFile[S <: SnapshotableState[S]](
  file: Path,
  length: Long,
  /** Last position in events section, but not in a transaction. */
  lastProperEventPosition: Long,
  /** File's JournalHeader. */
  journalHeader: JournalHeader,
  /** The calculated recovered JournalHeader to continue with. */
  nextJournalHeader: JournalHeader,
  /** File's position before the events. */
  firstEventPosition: Long,
  /** The recovered state */
  state: S):

  assertThat(journalHeader.journalId == nextJournalHeader.journalId)
  assertThat(journalHeader.eventId < nextJournalHeader.eventId)
  assertThat(journalHeader.totalEventCount < nextJournalHeader.totalEventCount)

  def journalId = journalHeader.journalId

  def fileEventId = journalHeader.eventId

  def eventId = nextJournalHeader.eventId

  def journalPosition = JournalPosition(fileEventId, length)

  override def toString = s"RecoveredJournalFile($file ($length bytes),$journalHeader,$firstEventPosition,$state,$nextJournalHeader)"
