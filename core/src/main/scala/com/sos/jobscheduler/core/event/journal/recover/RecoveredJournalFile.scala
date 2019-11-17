package com.sos.jobscheduler.core.event.journal.recover

import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.data.event.{Event, JournaledState}
import java.nio.file.Path
import scala.language.higherKinds

final case class RecoveredJournalFile[S <: JournaledState[S, E], E <: Event](
  file: Path,
  length: Long,
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

  override def toString = s"RecoveredJournalFile($file ($length bytes),$journalHeader,$firstEventPosition,$state,$calculatedJournalHeader)"
}
