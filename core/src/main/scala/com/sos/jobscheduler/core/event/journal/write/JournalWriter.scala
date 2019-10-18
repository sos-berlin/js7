package com.sos.jobscheduler.core.event.journal.write

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.common.utils.ByteUnits.toMB
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.EventHeader
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta}
import com.sos.jobscheduler.core.event.journal.write.EventJournalWriter.SerializationException
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import io.circe.syntax.EncoderOps
import java.nio.file.{Files, Path}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[journal] abstract class JournalWriter(after: EventId, append: Boolean)
extends AutoCloseable
{
  protected val journalMeta: JournalMeta
  def file: Path
  protected def simulateSync: Option[FiniteDuration]
  protected val statistics: StatisticsCounter
  private var _eventsStarted = append
  private var _lastEventId = after

  if (!append && Files.exists(file)) sys.error(s"JournalWriter: Not expecting already existing file '$file'")
  if (append && !Files.exists(file)) sys.error(s"JournalWriter: Missing file '$file'")

  protected final val jsonWriter = new FileJsonWriter(file, append = append, simulateSync = simulateSync)

  def close() = jsonWriter.close()

  final def writeHeader(header: JournalHeader): Unit = {
    jsonWriter.write(ByteString.fromString(header.asJson.compactPrint))
    flush(sync = false)
  }

  def beginEventSection(sync: Boolean): Unit = {
    if (_eventsStarted) throw new IllegalStateException("EventJournalWriter: duplicate beginEventSection()")
    jsonWriter.write(ByteString(EventHeader.compactPrint))
    flush(sync = sync)
    _eventsStarted = true
  }

  def writeEvent(stamped: Stamped[KeyedEvent[Event]]): Unit = {
    import journalMeta.eventJsonCodec
    if (stamped.eventId <= _lastEventId)
      throw new IllegalArgumentException(s"EventJournalWriter.writeEvent with EventId ${EventId.toString(stamped.eventId)} <= lastEventId ${EventId.toString(_lastEventId)}")
    _lastEventId = stamped.eventId
    val byteString =
      try ByteString(stamped.asJson.compactPrint)
      catch { case t: Exception => throw new SerializationException(t) }
    jsonWriter.write(byteString)
  }

  protected final def eventsStarted = _eventsStarted

  protected final def lastWrittenEventId = _lastEventId

  protected final def fileSizeString: String =
    try toMB(Files.size(file)) catch { case NonFatal(t) => t.toString }

  def flush(sync: Boolean): Unit = {
    if (!jsonWriter.isFlushed) {
      statistics.beforeFlush()
      jsonWriter.flush()
      statistics.afterFlush()
    }
    if (sync && !isSynced) {
      statistics.beforeSync()
      jsonWriter.sync()
      statistics.afterSync()
    }
  }

  final def isFlushed = jsonWriter.isFlushed

  final def isSynced = jsonWriter.isSynced

  final def fileLength = jsonWriter.fileLength

  final def bytesWritten = jsonWriter.bytesWritten
}
