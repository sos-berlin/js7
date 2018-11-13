package com.sos.jobscheduler.core.event.journal.write

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.{SnapshotFooter, SnapshotHeader}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles._
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.data.event.{Event, EventId}
import java.nio.file.Path
import java.time.Instant.now
import scala.concurrent.duration.FiniteDuration


/**
  * @author Joacim Zschimmer
  */
private[journal] final class SnapshotJournalWriter[E <: Event](
  journalMeta: JournalMeta[E],
  val file: Path,
  protected val observer: Option[JournalingObserver],
  protected val simulateSync: Option[FiniteDuration])
extends JournalWriter[E](append = false)
{
  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics = new SnapshotStatisticsCounter
  private var snapshotStarted = false
  private var snapshotCount = 0
  private val startedAt = now

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

  def endSnapshotSection(sync: Boolean): Unit = {
    jsonWriter.write(ByteString(SnapshotFooter.compactPrint))
    flush(sync = sync)
    logger.info(s"Snapshot finished, $fileSizeString written ($snapshotCount snapshot objects in ${(now - startedAt).pretty})")
    for (o ← statistics.debugString) logger.debug(o)
  }

  override def toString = s"SnapshotJournalWriter(${file.getFileName})"
}

object SnapshotJournalWriter
{
  def forTest[E <: Event](journalMeta: JournalMeta[E], after: EventId, observer: Option[JournalingObserver] = None) =
    new SnapshotJournalWriter[E](journalMeta, journalMeta.file(after), observer, simulateSync = None)
}
