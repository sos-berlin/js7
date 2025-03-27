package js7.journal.write

import cats.effect.unsafe.IORuntime
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.*
import js7.base.log.Logger
import js7.base.utils.Assertions.assertThat
import js7.common.jsonseq.PositionAnd
import js7.data.event.JournalSeparators.{Commit, Transaction}
import js7.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.extensions.file
import js7.journal.watch.JournalingObserver
import js7.journal.write.EventJournalWriter.*
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final class EventJournalWriter(
  journalLocation: JournalLocation,
  after: EventId,
  journalId: JournalId,
  observer: Option[JournalingObserver],
  protected val simulateSync: Option[FiniteDuration],
  append: Boolean = true,
  initialEventCount: Int = 0)
  (implicit protected val ioRuntime: IORuntime)
extends
  JournalWriter(
    journalLocation.S,
    journalLocation.file(after),
    after = after,
    append = append),
  AutoCloseable:

  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics: EventStatisticsCounter =
    new EventStatisticsCounter(initialEventCount)
  private var _eventWritten = false

  def closeProperly(sync: Boolean): Unit =
    try if eventsStarted then endEventSection(sync = sync)
    finally close()

  override def close(): Unit =
    super.close()
    for o <- observer do
      o.onJournalingEnded(jsonWriter.fileLength)
    for o <- statistics.debugString do logger.debug(o)

  def onJournalingStarted(fileLengthBeforeEvents: Long = jsonWriter.fileLength): Unit =
    assertThat(fileLengthBeforeEvents <= jsonWriter.fileLength)
    for o <- observer do
      val firstEventPositionAndFileEventId = PositionAnd(fileLengthBeforeEvents, lastWrittenEventId)
      o.onJournalingStarted(file, journalId,
        firstEventPositionAndFileEventId,
        firstEventPositionAndFileEventId,
        isActiveNode = true)
      o.onFileWritten(jsonWriter.fileLength)

  ///** For SnapshotTaken event written with SnapshotJournalWriter. */
  //def onInitialEventsWritten(): Unit = {
  //  for (o <- observer) {
  //    // Initially written events are not counted in statistics
  //    o.onFileWritten(jsonWriter.fileLength)
  //  }
  //}

  def writeEvents(stampedEvents: Seq[Stamped[KeyedEvent[Event]]], transaction: Boolean = false): Unit =
    // TODO Rollback writes in case of error (with seek?)
    if !eventsStarted then throw new IllegalStateException
    _eventWritten = true
    statistics.countEventsToBeCommitted(stampedEvents.size)
    val ta = transaction && stampedEvents.lengthIs > 1
    if ta then jsonWriter.write(TransactionByteArray)
    writeEvents_(stampedEvents)
    if ta then jsonWriter.write(CommitByteArray)

  // Event section begin has been written by SnapshotJournalWriter
  def endEventSection(sync: Boolean): Unit =
    if !eventsStarted then throw new IllegalStateException
    flush(sync = sync)
    notifyObserver()
    logger.debug(s"Journal finished, $fileSizeString written ($statistics)")

  override def flush(sync: Boolean): Unit =
    super.flush(sync)
    // TODO Notify observer first after sync! OrderStdWritten braucht dann und wann ein sync (1s), um observer nicht lange warten zu lassen.
    for r <- observer do
      r.onFileWritten(jsonWriter.fileLength)

  def notifyObserver(): Unit =
    assertThat(isFlushed)
    for r <- observer do
      r.onFileWritten(jsonWriter.fileLength)

  def onCommitted(lengthAndEventId: PositionAnd[EventId], n: Int): Unit =
    for r <- observer do
      r.onEventsCommitted(lengthAndEventId, n)

  def isEventWritten: Boolean = _eventWritten

  def fileLengthAndEventId: PositionAnd[EventId] =
    PositionAnd(fileLength, lastWrittenEventId)

  override def toString = s"EventJournalWriter(${file.getFileName})"


private[journal] object EventJournalWriter:
  private val TransactionByteArray = Transaction.asJson.toByteArray
  private val CommitByteArray = Commit.asJson.toByteArray

  def forTest(journalLocation: JournalLocation, after: EventId, journalId: JournalId,
    observer: Option[JournalingObserver] = None, append: Boolean = false)
    (using IORuntime)
  =
    new EventJournalWriter(journalLocation, after, journalId, observer,
      simulateSync = None, append = append)

  final class SerializationException(cause: Throwable) extends RuntimeException("JSON serialization error", cause)
