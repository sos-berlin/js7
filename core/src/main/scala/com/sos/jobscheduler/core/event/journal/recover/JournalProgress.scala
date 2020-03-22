package com.sos.jobscheduler.core.event.journal.recover

sealed trait JournalProgress
{
  def isAcceptingEvents = false
}

object JournalProgress
{
  case object Initial extends JournalProgress

  case object AfterHeader extends JournalProgress

  case object InSnapshotSection extends JournalProgress

  case object AfterSnapshotSection extends JournalProgress

  case object InCommittedEventsSection extends JournalProgress {
    override def isAcceptingEvents = true
  }

  case object InTransaction extends JournalProgress {
    override def isAcceptingEvents = true
  }

  case object EndOfFile extends JournalProgress
}
