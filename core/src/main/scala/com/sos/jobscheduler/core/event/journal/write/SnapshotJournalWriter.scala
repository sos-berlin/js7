package com.sos.jobscheduler.core.event.journal.write

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.{SnapshotFooter, SnapshotHeader}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles._
import com.sos.jobscheduler.data.event.EventId
import java.nio.file.Path
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
private[journal] final class SnapshotJournalWriter(
  protected val journalMeta: JournalMeta,
  val file: Path,
  after: EventId,
  protected val simulateSync: Option[FiniteDuration])
extends JournalWriter(after = after, append = false)
{
  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics = new SnapshotStatisticsCounter
  private var snapshotStarted = false
  private var snapshotCount = 0
  private val runningSince = now

  def closeAndLog(): Unit = {
    super.close()
    logger.debug(s"Snapshot finished, $fileSizeString written ($snapshotCount snapshot objects in ${runningSince.elapsed.pretty})")
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
  }

  override def toString = s"SnapshotJournalWriter(${file.getFileName})"
}

object SnapshotJournalWriter
{
  def forTest(journalMeta: JournalMeta, after: EventId) =
    new SnapshotJournalWriter(journalMeta, journalMeta.file(after), after =after, simulateSync = None)
}
