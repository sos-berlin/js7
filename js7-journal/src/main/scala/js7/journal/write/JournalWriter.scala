package js7.journal.write

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import java.nio.file.{Files, Path}
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.data.ByteArray
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.utils.ByteUnits.toMB
import js7.data.event.JournalSeparators.EventHeader
import js7.data.event.{Event, EventId, JournalHeader, JournaledState, KeyedEvent, Stamped}
import js7.journal.write.EventJournalWriter.SerializationException
import js7.journal.write.JournalWriter.*
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[journal] abstract class JournalWriter(
  S: JournaledState.HasEventCodec,
  after: EventId,
  append: Boolean)
extends AutoCloseable:

  def file: Path
  protected def simulateSync: Option[FiniteDuration]
  protected val statistics: StatisticsCounter
  protected def ioRuntime: IORuntime

  private var _eventsStarted = append
  private var _lastEventId = after

  if !append && Files.exists(file) then sys.error(s"JournalWriter: Unexpected journal file: $file")
  if append && !Files.exists(file) then sys.error(s"JournalWriter: Missing journal file: $file")

  protected final val jsonWriter = new FileJsonWriter(file, append = append, simulateSync = simulateSync)

  def close(): Unit = 
    jsonWriter.close()

  final def writeHeader(header: JournalHeader): Unit =
    jsonWriter.write(header.asJson.toByteArray)
    flush(sync = false)

  def beginEventSection(sync: Boolean): Unit =
    if _eventsStarted then throw new IllegalStateException("EventJournalWriter: duplicate beginEventSection()")
    jsonWriter.write(EventHeader.toByteArray)
    flush(sync = sync)
    _eventsStarted = true

  def writeEvent(stamped: Stamped[KeyedEvent[Event]]): Unit =
    writeEvents_(stamped :: Nil)

  protected def writeEvents_(stampedEvents: Seq[Stamped[KeyedEvent[Event]]]): Unit =
    for stamped <- stampedEvents do
      if stamped.eventId <= _lastEventId then throw new IllegalArgumentException(
        s"EventJournalWriter.writeEvent with EventId ${EventId.toString(stamped.eventId)}" +
          s" <= lastEventId ${EventId.toString(_lastEventId)}")
      _lastEventId = stamped.eventId
    import S.keyedEventJsonCodec
    if sys.runtime.availableProcessors > 1 && stampedEvents.sizeIs >= JsonParallelizationThreshold then
      writeJsonInParallel(stampedEvents)
    else
      writeJsonSerially(stampedEvents)

  private def writeJsonSerially[A: Encoder](seq: Seq[A]): Unit =
    for a <- seq do jsonWriter.write(serialize(a))

  private def writeJsonInParallel[A: Encoder](seq: Seq[A]): Unit =
    // TODO Try to call it asynchronously (in JournalActor)
    given IORuntime = ioRuntime
    Stream.iterable[IO, A](seq)
      .mapParallelBatch():
        serialize[A]
      .foreach: byteArray =>
        IO(jsonWriter.write(byteArray))
      .compile.drain
      .unsafeRunSync() /*Blocking !!!*/

  private def serialize[A: Encoder](a: A): ByteArray =
    try a.asJson.toByteArray
    catch { case t: Exception => throw new SerializationException(t) }

  protected final def eventsStarted = _eventsStarted

  protected final def lastWrittenEventId = _lastEventId

  protected final def fileSizeString: String =
    try toMB(Files.size(file)) catch { case NonFatal(t) => t.toString }

  def flush(sync: Boolean): Unit =
    if !jsonWriter.isFlushed then
      statistics.beforeFlush()
      jsonWriter.flush()
      statistics.afterFlush()
    if sync && !isSynced then
      statistics.beforeSync()
      jsonWriter.sync()
      statistics.afterSync()

  final def isFlushed = jsonWriter.isFlushed

  final def isSynced = jsonWriter.isSynced

  final def fileLength = jsonWriter.fileLength

  final def bytesWritten = jsonWriter.bytesWritten


object JournalWriter:
  private val JsonBatchSize = 256
  private val JsonParallelizationThreshold = 3 * JsonBatchSize
