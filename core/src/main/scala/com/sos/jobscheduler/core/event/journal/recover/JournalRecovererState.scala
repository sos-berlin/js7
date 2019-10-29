package com.sos.jobscheduler.core.event.journal.recover

private[recover] sealed trait JournalRecovererState
{
  //private def apply(json: Json): Checked[JournalRecovererState] = {
  //  (this, json) match {
  //    case (Start, _) if json.as[JournalHeader].isRight => Right(AfterHeader)
  //    case (AfterHeader, SnapshotHeader) => Right(InSnapshotSection)
  //    case (InSnapshotSection, SnapshotFooter) => Right(AfterSnapshotSection)
  //    case (AfterSnapshotSection, EventHeader) => Right(InEventsSection)
  //    case (InEventsSection, EventFooter) => Right(AfterEventsSection)
  //    case (InEventsSection, Transaction) => Right(InTransaction)
  //    case (InTransaction, Commit) => Right(InEventsSection)
  //    case (_, EndOfJournalFileMarkerJson/*???*/) => Right(EndOfFile)
  //    case _ => Left(Problem(s"Unexpected line in journal file while in JournalRecovererState $toString: ${json.compactPrint.truncateWithEllipsis(50)}"))
  //  }
  //}
}

private[recover] object JournalRecovererState
{
  def apply(): JournalRecovererState = Start

  case object Start extends JournalRecovererState

  case object AfterHeader extends JournalRecovererState

  case object InSnapshotSection extends JournalRecovererState

  case object AfterSnapshotSection extends JournalRecovererState

  case object InEventsSection extends JournalRecovererState

  case object AfterEventsSection extends JournalRecovererState

  case object InTransaction extends JournalRecovererState

  case object EndOfFile extends JournalRecovererState
}
