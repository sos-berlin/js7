package js7.journal.recover

sealed trait JournalProgress


object JournalProgress:
  case object Initial extends JournalProgress

  case object AfterHeader extends JournalProgress

  case object InSnapshotSection extends JournalProgress

  case object AfterSnapshotSection extends JournalProgress

  case object InCommittedEventsSection extends JournalProgress

  case object InTransaction extends JournalProgress

  //case object EndOfFile extends JournalProgress
