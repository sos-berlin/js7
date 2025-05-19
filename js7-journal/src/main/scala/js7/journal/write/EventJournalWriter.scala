package js7.journal.write

import cats.effect.unsafe.IORuntime
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.*
import js7.base.log.Logger
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Missing
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
  fileEventId: EventId,
  after: EventId,
  journalId: JournalId,
  observer: JournalingObserver,
  protected val simulateSync: Option[FiniteDuration],
  append: Boolean = true,
  initialEventCount: Int = 0)
  (implicit protected val ioRuntime: IORuntime)
extends
  JournalWriter(
    journalLocation.S,
    journalLocation.file(fileEventId),
    after = after,
    append = append,
    initialEventCount = initialEventCount),
  AutoCloseable:

  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics: EventStatisticsCounter =
    new EventStatisticsCounter(initialEventCount)
  private var _committedEventCount = 0L

  def closeProperly(sync: Boolean): Unit =
    try if eventsStarted then endEventSection(sync = sync)
    finally close()

  override def close(): Unit =
    super.close()
    observer.onJournalingEnded(jsonWriter.fileLength)
    for o <- statistics.debugString do logger.debug(o)

  def onJournalingStarted(fileLengthBeforeEvents: Long = jsonWriter.fileLength): Unit =
    assertThat(fileLengthBeforeEvents <= jsonWriter.fileLength)
    observer.onJournalingStarted(file, journalId,
      firstEventPositionAndFileEventId = PositionAnd(fileLengthBeforeEvents, fileEventId),
      // Must have been flushed!
      flushedLengthAndEventId = PositionAnd(fileLength, lastWrittenEventId),
      isActiveNode = true)

  def writeEvents(stampedEvents: Seq[Stamped[KeyedEvent[Event]]], transaction: Boolean = false)
  : PositionAnd[EventId] =
    // TODO Rollback writes in case of error (with seek?)
    if !eventsStarted then throw new IllegalStateException
    val ta = transaction && stampedEvents.lengthIs > 1
    //logger.trace(s"### writeEvents ${ta ?? "transaction "}${
    //  stampedEvents.map(o => s"${o.eventId}:${o.value.event.getClass.simpleScalaName}")
    //    .mkString("[", ", ", "]")}")
    if ta then jsonWriter.write(TransactionByteArray)
    super.writeEvents(stampedEvents)
    if ta then jsonWriter.write(CommitByteArray)
    statistics.countEventsToBeCommitted(stampedEvents.size)
    fileLengthAndEventId

  // Event section begin has been written by SnapshotJournalWriter
  def endEventSection(sync: Boolean): Unit =
    if !eventsStarted then throw new IllegalStateException
    flush(sync = sync)
    logger.debug(s"Journal finished, $fileSizeString written ($statistics)")

  override def flush(sync: Boolean): Unit =
    if !isFlushed || sync && !isSynced then
      super.flush(sync)
      observer.onFileWritten(jsonWriter.fileLength)
    end if

  /** @param n estimated number of events
    */
  def onCommitted(lengthAndEventId: PositionAnd[EventId], n: Int): Unit =
    _committedEventCount = eventCount
    observer.onEventsCommitted(lengthAndEventId, n)

  def fileLengthAndEventId: PositionAnd[EventId] =
    PositionAnd(fileLength, lastWrittenEventId)

  override def toString = s"EventJournalWriter(${file.getFileName})"


private[journal] object EventJournalWriter:
  private val TransactionByteArray = Transaction.asJson.toByteArray
  private val CommitByteArray = Commit.asJson.toByteArray

  def forTest(journalLocation: JournalLocation, after: EventId, journalId: JournalId,
    observer: JournalingObserver | Missing = Missing,
    append: Boolean = false)
    (using IORuntime)
  : EventJournalWriter =
    new EventJournalWriter(journalLocation, fileEventId = after, after = after, journalId,
      observer getOrElse JournalingObserver.Dummy(journalLocation),
      simulateSync = None, append = append)

  final class SerializationException(cause: Throwable)
    extends RuntimeException("JSON serialization error", cause)
