package js7.journal.write

import io.circe.syntax.EncoderOps
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.*
import js7.base.log.Logger
import js7.base.utils.Assertions.assertThat
import js7.common.jsonseq.PositionAnd
import js7.data.event.JournalSeparators.{Commit, Transaction}
import js7.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles.*
import js7.journal.watch.JournalingObserver
import js7.journal.write.EventJournalWriter.*
import monix.execution.Scheduler
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final class EventJournalWriter(
  protected val journalMeta: JournalMeta,
  val file: Path,
  after: EventId,
  journalId: JournalId,
  observer: Option[JournalingObserver],
  protected val simulateSync: Option[FiniteDuration],
  withoutSnapshots: Boolean = false,
  initialEventCount: Int = 0)
  (implicit protected val scheduler: Scheduler)
extends JournalWriter(after = after, append = !withoutSnapshots)
with AutoCloseable
{
  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics: EventStatisticsCounter =
    new EventStatisticsCounter(initialEventCount)
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
      val firstEventPositionAndFileEventId = PositionAnd(fileLengthBeforeEvents, lastWrittenEventId)
      o.onJournalingStarted(file, journalId,
        firstEventPositionAndFileEventId,
        firstEventPositionAndFileEventId,
        isActiveNode = true)
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
    val ta = transaction && stampedEvents.lengthIs > 1
    if (ta) jsonWriter.write(TransactionByteArray)
    writeEvents_(stampedEvents)
    if (ta) jsonWriter.write(CommitByteArray)
  }

  // Event section begin has been written by SnapshotJournalWriter
  def endEventSection(sync: Boolean): Unit = {
    if (!eventsStarted) throw new IllegalStateException
    flush(sync = sync)
    notifyObserver()
    logger.debug(s"Journal finished, $fileSizeString written ($statistics)")
  }

  override def flush(sync: Boolean): Unit = {
    super.flush(sync)
    // TODO Notify observer first after sync! OrderStdWritten braucht dann und wann ein sync (1s), um observer nicht lange warten zu lassen.
    for (r <- observer) {
      r.onFileWritten(jsonWriter.fileLength)
    }
  }

  def notifyObserver(): Unit = {
    assertThat(isFlushed)
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
  private val TransactionByteArray = Transaction.asJson.toByteArray
  private val CommitByteArray = Commit.asJson.toByteArray

  def forTest(journalMeta: JournalMeta, after: EventId, journalId: JournalId,
    observer: Option[JournalingObserver] = None, withoutSnapshots: Boolean = true)
    (implicit scheduler: Scheduler)
  =
    new EventJournalWriter(journalMeta, journalMeta.file(after), after, journalId, observer,
      simulateSync = None, withoutSnapshots = withoutSnapshots)

  final class SerializationException(cause: Throwable) extends RuntimeException("JSON serialization error", cause)
}
