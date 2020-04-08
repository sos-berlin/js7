package com.sos.jobscheduler.core.event.journal.write

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.common.event.PositionAnd
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.{Commit, Transaction}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles._
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.write.EventJournalWriter._
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import io.circe.syntax.EncoderOps
import java.nio.file.Path
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
private[journal] final class EventJournalWriter(
  protected val journalMeta: JournalMeta,
  val file: Path,
  after: EventId,
  journalId: JournalId,
  observer: Option[JournalingObserver],
  protected val simulateSync: Option[FiniteDuration],
  withoutSnapshots: Boolean = false,
  initialEventCount: Int = 0)
extends JournalWriter(after = after, append = !withoutSnapshots)
with AutoCloseable
{
  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics = new EventStatisticsCounter(initialEventCount)
  private var _eventWritten = false

  def closeProperly(sync: Boolean): Unit =
    try if (eventsStarted) endEventSection(sync = sync)
    finally close()

  override def close() = {
    super.close()
    for (o <- observer) {
      o.onJournalingEnded(jsonWriter.fileLength)
    }
    for (o <- statistics.debugString) logger.debug(o)
  }

  def onJournalingStarted(fileLengthBeforeEvents: Long = jsonWriter.fileLength): Unit = {
    assertThat(fileLengthBeforeEvents <= jsonWriter.fileLength)
    for (o <- observer) {
      val lengthAndEventId = PositionAnd(fileLengthBeforeEvents, lastWrittenEventId)
      o.onJournalingStarted(file, journalId, lengthAndEventId, lengthAndEventId)
      o.onFileWritten(jsonWriter.fileLength)
    }
  }

  ///** For SnapshotTaken event written with SnapshotJournalWriter. */
  //def onInitialEventsWritten(): Unit = {
  //  for (o <- observer) {
  //    // Initially written events are not counted in statistics
  //    o.onFileWritten(jsonWriter.fileLength)
  //  }
  //}

  def writeEvents(stampedEvents: Seq[Stamped[KeyedEvent[Event]]], transaction: Boolean = false): Unit = {
    // TODO Rollback writes in case of error (with seek?)
    if (!eventsStarted) throw new IllegalStateException
    _eventWritten = true
    statistics.countEventsToBeCommitted(stampedEvents.size)
    val ta = transaction && stampedEvents.lengthCompare(1) > 0
    if (ta) jsonWriter.write(TransactionByteString)
    stampedEvents foreach writeEvent
    if (ta) jsonWriter.write(CommitByteString)
  }

  // Event section begin has been written by SnapshotJournalWriter
  def endEventSection(sync: Boolean): Unit = {
    if (!eventsStarted) throw new IllegalStateException
    flush(sync = sync)
    logger.debug(s"Journal finished, $fileSizeString written ($statistics)")
  }

  override def flush(sync: Boolean): Unit = {
    super.flush(sync)
    // TODO Notify observer first after sync! OrderStdWritten braucht dann und wann ein sync (1s), um observer nicht lange warten zu lassen.
    for (r <- observer) {
      r.onFileWritten(jsonWriter.fileLength)
    }
  }

  def onCommitted(lengthAndEventId: PositionAnd[EventId], n: Int): Unit = {
    for (r <- observer) {
      r.onEventsCommitted(lengthAndEventId, n)
    }
  }

  def isEventWritten = _eventWritten

  def fileLengthAndEventId = PositionAnd(fileLength, lastWrittenEventId)

  override def toString = s"EventJournalWriter(${file.getFileName})"
}

private[journal] object EventJournalWriter
{
  private val TransactionByteString = ByteString(Transaction.asJson.compactPrint)
  private val CommitByteString = ByteString(Commit.asJson.compactPrint)

  def forTest(journalMeta: JournalMeta, after: EventId, journalId: JournalId,
    observer: Option[JournalingObserver] = None, withoutSnapshots: Boolean = true)
  =
    new EventJournalWriter(journalMeta, journalMeta.file(after), after, journalId, observer,
      simulateSync = None, withoutSnapshots = withoutSnapshots)

  final class SerializationException(cause: Throwable) extends RuntimeException("JSON serialization error", cause)
}
