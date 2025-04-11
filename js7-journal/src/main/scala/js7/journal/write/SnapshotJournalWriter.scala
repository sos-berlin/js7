package js7.journal.write

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import io.circe.syntax.EncoderOps
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.{Files, Path}
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.common.jsonseq.PositionAnd
import js7.data.event.JournalEvent.SnapshotTaken
import js7.data.event.JournalSeparators.{SnapshotFooter, SnapshotHeader}
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{EventId, JournalEvent, JournalHeader, KeyedEvent, SnapshotableState, Stamped}
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.extensions.file
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class SnapshotJournalWriter(
  S: SnapshotableState.HasCodec,
  file: Path,
  after: EventId,
  protected val simulateSync: Option[FiniteDuration])
  (using protected val ioRuntime: IORuntime)
extends JournalWriter(S, file, after = after, append = false):

  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics: SnapshotStatisticsCounter = new SnapshotStatisticsCounter
  private var snapshotStarted = false
  private var snapshotCount = 0
  private val runningSince = now

  def closeAndLog(): Unit =
    super.close()
    val elapsed = runningSince.elapsed
    logger.debug("Snapshot finished - " + itemsPerSecondString(elapsed, snapshotCount, "objects") +
      " · " + bytesPerSecondString(elapsed, fileLength))
    for o <- statistics.debugString do logger.info(o)

  def beginSnapshotSection(): Unit =
    if snapshotStarted then
      throw new IllegalStateException("SnapshotJournalWriter: duplicate beginSnapshotSection()")
    jsonWriter.write(SnapshotHeader.toByteArray)
    flush(sync = false)
    snapshotStarted = true

  private def writeSnapshotStream(snapshotStream: fs2.Stream[fs2.Pure, Any]): IO[Unit] =
    snapshotStream
      .filter:
        case SnapshotEventId(_) => false // JournalHeader contains already the EventId
        case _ => true
      .mapParallelBatch(): snapshotObject =>
        //logger.trace(s"Snapshot ${snapshotObject.toString.truncateWithEllipsis(200)}")
        snapshotObject.asJson(S.snapshotObjectJsonCodec).toByteArray
      .foreach: byteArray =>
        IO.blocking:
          writeSnapshot(byteArray)
      .compile
      .drain

  def writeSnapshot(json: ByteArray): Unit =
    if !snapshotStarted then throw new IllegalStateException(
      "SnapshotJournalWriter: writeSnapshots(), but snapshots have not been started")
    statistics.countSnapshot()
    jsonWriter.write(json)
    snapshotCount += 1

  def endSnapshotSection(): Unit =
    jsonWriter.write(SnapshotFooter.toByteArray)
    statistics.setFileLength(jsonWriter.fileLength)

  override def toString = s"SnapshotJournalWriter(${file.getFileName})"


object SnapshotJournalWriter:

  def forTest(journalLocation: JournalLocation, after: EventId)(using IORuntime) =
    new SnapshotJournalWriter(
      journalLocation.S,
      journalLocation.file(after),
      after = after, simulateSync = None)

  /** Write a complete journal file with a snapshot.
    * @return (position of the first event (SnapshotTaken), journal file's EventId) */
  def writeSnapshotStream[S <: SnapshotableState[S]](
    S: SnapshotableState.HasCodec,
    file: Path,
    journalHeader: JournalHeader,
    snapshotStream: fs2.Stream[fs2.Pure, Any],
    snapshotTaken: Stamped[KeyedEvent[SnapshotTaken]],
    syncOnCommit: Boolean = false,
    simulateSync: Option[FiniteDuration] = None)
    (using IORuntime)
  : IO[PositionAnd[EventId]] =
    val tmpFile = JournalLocation.toTemporaryFile(file)
    Resource(IO.blocking:
      val w = new SnapshotJournalWriter(
        S, tmpFile,
        after = journalHeader.eventId,
        simulateSync = simulateSync)
      w -> IO:
        w.closeAndLog())
    .use: w =>
      IO.blocking:
        w.writeHeader(journalHeader)
        w.beginSnapshotSection()
      .productR:
        w.writeSnapshotStream(snapshotStream)
      .productR:
        IO.blocking:
          w.endSnapshotSection()

          // Write a SnapshotTaken event to increment EventId to
          // get a new (EventId-based) filename for the next journal file
          w.beginEventSection(sync = false)
          val firstEventPositionAndEventId = w.fileLengthAndEvenId
          w.writeEvent(snapshotTaken)
          w.flush(sync = syncOnCommit)
          firstEventPositionAndEventId
    .flatTap: _ =>
      IO.blocking:
        Files.move(tmpFile, file, ATOMIC_MOVE)
