package js7.journal.file.write

import io.circe.Encoder
import io.circe.syntax.EncoderOps
import java.nio.file.{Files, Path}
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.data.ByteArray
import js7.base.metering.CallMeter
import js7.base.utils.ByteUnits.toMB
import js7.common.jsonseq.PositionAnd
import js7.data.event.JournalSeparators.EventHeader
import js7.data.event.{Event, EventId, JournalHeader, JournalSeparators, JournaledState, KeyedEvent, Stamped}
import js7.journal.file.FileJournalMXBean
import js7.journal.file.write.EventJournalWriter.SerializationException
import js7.journal.file.write.JournalWriter.*
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[journal] abstract class JournalWriter(
  S: JournaledState.HasEventCodec,
  val file: Path,
  bean: FileJournalMXBean.Bean,
  after: EventId,
  append: Boolean,
  initialEventCount: Long = 0)
extends AutoCloseable:

  protected def simulateSync: Option[FiniteDuration]
  protected val statistics: StatisticsCounter

  private var _eventsStarted = append
  private var _lastEventId = after
  private var _eventCount = initialEventCount

  if !append && Files.exists(file) then sys.error(s"JournalWriter: Unexpected journal file: $file")
  if append && !Files.exists(file) then sys.error(s"JournalWriter: Missing journal file: $file")

  protected final val jsonWriter = new FileJsonWriter(file, append = append, simulateSync = simulateSync)

  def close(): Unit =
    jsonWriter.close()

  final def writeHeader(header: JournalHeader): Unit =
    jsonWriter.write(header.asJson.toByteArray)
    flush(sync = false)

  def beginEventSection(sync: Boolean): Unit =
    if _eventsStarted then
      throw IllegalStateException("EventJournalWriter: duplicate beginEventSection()")
    jsonWriter.write(EventHeader.toByteArray)
    flush(sync = sync)
    _eventsStarted = true

  def writeEventSync(stamped: Stamped[KeyedEvent[Event]]): Unit =
    writeEventsSync_(stamped :: Nil, transaction = false)

  /** Synchronous variant of `writeEvents_`, to be called in a blocking section.
    * Allows the caller to write many chunks with a single `IO.blocking`. */
  protected final def writeEventsSync_(
    stampedEvents: Seq[Stamped[KeyedEvent[Event]]],
    transaction: Boolean)
  : Unit =
    _eventCount += stampedEvents.size

    for stamped <- stampedEvents do
      if stamped.eventId <= _lastEventId then throw IllegalArgumentException:
        s"JournalWriter.writeEvent with EventId ${EventId.toString(stamped.eventId)}" +
          s" <= lastEventId ${EventId.toString(_lastEventId)}: ${stamped.value}"
      _lastEventId = stamped.eventId

    import S.keyedEventJsonCodec
    val ta = transaction && stampedEvents.lengthIs > 1
    writeJsonSerially_(stampedEvents, ta)

  private def countEvents(
    stampedEvents: Seq[Stamped[KeyedEvent[Event]]],
    transaction: Boolean)
  : Boolean =
    _eventCount += stampedEvents.size
    for stamped <- stampedEvents do
      if stamped.eventId <= _lastEventId then throw IllegalArgumentException:
        s"JournalWriter.writeEvent with EventId ${EventId.toString(stamped.eventId)}" +
          s" <= lastEventId ${EventId.toString(_lastEventId)}: ${stamped.value}"
      _lastEventId = stamped.eventId
    transaction && stampedEvents.lengthIs > 1

  private def writeJsonSerially_[A: Encoder](seq: Seq[A], transaction: Boolean): Unit =
    if transaction then jsonWriter.write(TransactionByteArray)
    for a <- seq do jsonWriter.write(serialize(a))
    if transaction then jsonWriter.write(CommitByteArray)

  //private def writeJsonInParallel[A: Encoder](seq: Seq[A], transaction: Boolean): IO[Unit] =
  //  Stream.iterable[IO, A](seq)
  //    .mapParallelBatch():
  //      serialize[A]
  //    .pipeIf(transaction):
  //      _.prependOne(TransactionByteArray)
  //        .appendOne(CommitByteArray)
  //    .foreach: byteArray =>
  //      IO(jsonWriter.write(byteArray))
  //    .compile.drain

  private def serialize[A: Encoder](a: A): ByteArray =
    try a.asJson.toByteArray
    catch { case t: Exception => throw new SerializationException(t) }

  protected final def eventsStarted = _eventsStarted

  final def lastWrittenEventId: EventId =
    _lastEventId

  protected final def fileSizeString: String =
    try toMB(Files.size(file)) catch { case NonFatal(t) => t.toString }

  def flush(sync: Boolean): Unit =
    try
      if !jsonWriter.isFlushed then
        statistics.beforeFlush()
        meterFlush:
          jsonWriter.flush()
        statistics.afterFlush()
      if sync && !isSynced then
        statistics.beforeSync()
        meterSync:
          jsonWriter.sync()
        statistics.afterSync()
    catch case NonFatal(t) =>
      throw new RuntimeException(s"Error while writing to journal file", t)
    bean.flushTotal += 1

  final def isFlushed: Boolean =
    jsonWriter.isFlushed

  final def isSynced: Boolean =
    jsonWriter.isSynced

  final def fileLength: Long =
    jsonWriter.fileLength

  final def fileLengthAndEvenId: PositionAnd[EventId] =
    PositionAnd(jsonWriter.fileLength, lastWrittenEventId)

  final def bytesWritten: Long =
    jsonWriter.bytesWritten

  def eventCount: Long =
    _eventCount

object JournalWriter:
  private val JsonBatchSize = 256
  private val JsonParallelizationThreshold = 3 * JsonBatchSize

  private[journal] val TransactionByteArray = JournalSeparators.Transaction.asJson.toByteArray
  private[journal] val CommitByteArray = JournalSeparators.Commit.asJson.toByteArray

  private val meterFlush = CallMeter("JournalWriter.flush")
  private val meterSync = CallMeter("JournalWriter.sync")
