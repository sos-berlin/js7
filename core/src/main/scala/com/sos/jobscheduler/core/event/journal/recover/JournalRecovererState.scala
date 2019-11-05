package com.sos.jobscheduler.core.event.journal.recover

sealed trait JournalRecovererState
{
  def isAcceptingEvents = false
}

object JournalRecovererState
{
  def apply(): JournalRecovererState = Initial

  case object Initial extends JournalRecovererState

  case object AfterHeader extends JournalRecovererState

  case object InSnapshotSection extends JournalRecovererState

  case object AfterSnapshotSection extends JournalRecovererState

  case object InEventsSection extends JournalRecovererState {
    override def isAcceptingEvents = true
  }

  case object InTransaction extends JournalRecovererState {
    override def isAcceptingEvents = true
  }

  case object AfterEventsSection extends JournalRecovererState

  case object EndOfFile extends JournalRecovererState
}
