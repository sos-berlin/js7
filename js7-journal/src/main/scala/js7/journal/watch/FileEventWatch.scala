package js7.journal.watch

import cats.effect.IO
import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalPosition}
import scala.concurrent.duration.FiniteDuration

trait FileEventWatch extends EventWatch:

  def streamFile(journalPosition: JournalPosition,
    timeout: Option[FiniteDuration], markEOF: Boolean = false, onlyAcks: Boolean = false,
    chunkContentSize: Int)
  : IO[Checked[fs2.Stream[IO, fs2.Chunk[PositionAnd[ByteArray]]]]]

  def rawSnapshotAfter(after: EventId, chunkContentLimit: Int)
  : Option[fs2.Stream[IO, fs2.Chunk[ByteArray]]]

  def journalPosition: Checked[JournalPosition]

  def fileEventIds: Seq[EventId]

  final def tornEventId: EventId =
    fileEventIds.headOption getOrElse EventId.BeforeFirst

  final def lastFileEventId: EventId =
    fileEventIds.last

  def strict: StrictEventWatch =
    new StrictEventWatch(this)
