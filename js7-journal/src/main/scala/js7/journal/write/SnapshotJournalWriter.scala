package js7.journal.write

import java.nio.file.Path
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.data.event.{EventId, SnapshotableState}
import js7.data.event.JournalSeparators.{SnapshotFooter, SnapshotHeader}
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles.*
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class SnapshotJournalWriter(
  S: SnapshotableState.HasCodec,
  val file: Path,
  after: EventId,
  protected val simulateSync: Option[FiniteDuration])
  (implicit protected val scheduler: Scheduler)
extends JournalWriter(S, after = after, append = false)
{
  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics: SnapshotStatisticsCounter = new SnapshotStatisticsCounter
  private var snapshotStarted = false
  private var snapshotCount = 0
  private val runningSince = now

  def closeAndLog(): Unit = {
    super.close()
    val elapsed = runningSince.elapsed
    logger.debug(s"Snapshot finished - " + itemsPerSecondString(elapsed, snapshotCount, "objects") + " · " +
      bytesPerSecondString(elapsed, fileLength))
    for (o <- statistics.debugString) logger.info(o)
  }

  def beginSnapshotSection(): Unit = {
    if (snapshotStarted) throw new IllegalStateException("SnapshotJournalWriter: duplicate beginSnapshotSection()")
    jsonWriter.write(SnapshotHeader.toByteArray)
    flush(sync = false)
    snapshotStarted = true
  }

  def writeSnapshot(json: ByteArray): Unit = {
    if (!snapshotStarted) throw new IllegalStateException("SnapshotJournalWriter: writeSnapshots(), but snapshots have not been started")
    statistics.countSnapshot()
    jsonWriter.write(json)
    snapshotCount += 1
  }

  def endSnapshotSection(): Unit = {
    jsonWriter.write(SnapshotFooter.toByteArray)
    statistics.setFileLength(jsonWriter.fileLength)
  }

  override def toString = s"SnapshotJournalWriter(${file.getFileName})"
}

object SnapshotJournalWriter
{
  def forTest(journalMeta: JournalMeta, after: EventId)(implicit scheduler: Scheduler) =
    new SnapshotJournalWriter(journalMeta.S, journalMeta.file(after), after = after, simulateSync = None)
}
