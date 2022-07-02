package js7.journal.watch

import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalPosition}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration

trait FileEventWatch extends EventWatch
{
  def observeFile(journalPosition: JournalPosition,
    timeout: FiniteDuration, markEOF: Boolean = false, onlyAcks: Boolean = false)
  : Task[Checked[Observable[PositionAnd[ByteArray]]]]

  def rawSnapshotAfter(after: EventId): Option[Observable[ByteArray]]

  def journalPosition: Checked[JournalPosition]

  def fileEventIds: Seq[EventId]

  final def tornEventId =
    fileEventIds.headOption getOrElse EventId.BeforeFirst

  final def lastFileEventId =
    fileEventIds.last

  def strict: StrictEventWatch =
    new StrictEventWatch(this)
}
