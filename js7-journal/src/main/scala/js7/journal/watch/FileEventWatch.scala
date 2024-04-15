package js7.journal.watch

import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalPosition}
import cats.effect.IO
import fs2.Stream
import scala.concurrent.duration.FiniteDuration

trait FileEventWatch extends EventWatch:

  def streamFile(journalPosition: JournalPosition,
    timeout: FiniteDuration, markEOF: Boolean = false, onlyAcks: Boolean = false)
  : IO[Checked[Stream[IO, PositionAnd[ByteArray]]]]

  def rawSnapshotAfter(after: EventId): Option[Stream[IO, ByteArray]]

  def journalPosition: Checked[JournalPosition]

  def fileEventIds: Seq[EventId]

  final def tornEventId: EventId =
    fileEventIds.headOption getOrElse EventId.BeforeFirst

  final def lastFileEventId: EventId =
    fileEventIds.last

  def strict: StrictEventWatch =
    new StrictEventWatch(this)
