package js7.core.event.journal.write

import akka.util.ByteString
import java.nio.file.Path
import js7.base.circeutils.CirceUtils._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.common.scalautil.Logger
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.files.JournalFiles._
import js7.data.event.EventId
import js7.data.event.JournalSeparators.{SnapshotFooter, SnapshotHeader}
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class SnapshotJournalWriter(
  protected val journalMeta: JournalMeta,
  val file: Path,
  after: EventId,
  protected val simulateSync: Option[FiniteDuration])
  (implicit protected val scheduler: Scheduler)
extends JournalWriter(after = after, append = false)
{
  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics = new SnapshotStatisticsCounter
  private var snapshotStarted = false
  private var snapshotCount = 0
  private val runningSince = now

  def closeAndLog(): Unit = {
    super.close()
    logger.debug(s"Snapshot finished - " + itemsPerSecondString(runningSince.elapsed, snapshotCount, "objects"))
    for (o <- statistics.debugString) logger.debug(o)
  }

  def beginSnapshotSection(): Unit = {
    if (snapshotStarted) throw new IllegalStateException("SnapshotJournalWriter: duplicate beginSnapshotSection()")
    jsonWriter.write(ByteString(SnapshotHeader.compactPrint))
    flush(sync = false)
    snapshotStarted = true
  }

  def writeSnapshot(json: ByteString): Unit = {
    if (!snapshotStarted) throw new IllegalStateException("SnapshotJournalWriter: writeSnapshots(), but snapshots have not been started")
    statistics.countSnapshot()
    jsonWriter.write(json)
    snapshotCount += 1
  }

  def endSnapshotSection(): Unit = {
    jsonWriter.write(ByteString(SnapshotFooter.compactPrint))
    statistics.setFileLength(jsonWriter.fileLength)
  }

  override def toString = s"SnapshotJournalWriter(${file.getFileName})"
}

object SnapshotJournalWriter
{
  def forTest(journalMeta: JournalMeta, after: EventId)(implicit scheduler: Scheduler) =
    new SnapshotJournalWriter(journalMeta, journalMeta.file(after), after = after, simulateSync = None)
}
