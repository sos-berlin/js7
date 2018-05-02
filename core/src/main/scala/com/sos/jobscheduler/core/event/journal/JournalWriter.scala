package com.sos.jobscheduler.core.event.journal

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.ByteUnits.toMB
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.JournalWriter._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import io.circe.Json
import io.circe.syntax.EncoderOps
import java.nio.file.{Files, Path}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[journal] final class JournalWriter[E <: Event](
  meta: JournalMeta[E],
  val file: Path,
  appendToSnapshots: Boolean = false,
  eventReader: Option[JournalEventReader[E]])
extends AutoCloseable
{
  import meta.eventJsonCodec

  private val jsonWriter = new FileJsonWriter(JournalMeta.header, file, append = appendToSnapshots)
  private var positionsAndEventIds = mutable.Buffer[PositionAnd[EventId]]()
  private var snapshotStarted = appendToSnapshots
  private var eventsStarted = false
  private var _eventWritten = false
  private var _lastEventId = EventId.BeforeFirst
  private val statistics = new StatisticsCounter

  def close() = jsonWriter.close()

  def logStatistics(): Unit = {
    logger.info((try toMB(Files.size(file)) catch { case NonFatal(t) ⇒ t.toString }) + s" written. $statistics")
    logger.debug(statistics.timingString)
  }

  def logTiming(): Unit =
    logger.debug(statistics.timingString)

  def startSnapshots(lastEventId: EventId): Unit = {
    if (snapshotStarted) throw new IllegalStateException("JournalWriter: duplicate startSnapshots()")
    jsonWriter.write(ByteString(SnapshotsHeader.compactPrint))
    jsonWriter.write(ByteString(SnapshotMeta(lastEventId).asJson.compactPrint))
    flush()
    snapshotStarted = true
    _lastEventId = lastEventId
  }

  def writeSnapshot(json: ByteString): Unit = {
    if (!snapshotStarted) throw new IllegalStateException("JournalWriter: writeSnapshots(), but snapshots have not been started")
    if (eventsStarted) throw new IllegalStateException("JournalWriter: writeSnapshots(), but events have alreasy been started")
    jsonWriter.write(json)
  }

  def startEvents(): Unit = {
    if (eventsStarted) throw new IllegalStateException("JournalWriter: duplicate startEvents()")
    jsonWriter.write(ByteString(EventsHeader.compactPrint))
    flush()
    for (e ← eventReader) e.onJournalingStarted(PositionAnd(jsonWriter.fileLength, _lastEventId))
    eventsStarted = true
  }

  def writeEvents(stampedEvents: Seq[Stamped[KeyedEvent[E]]]): Unit = {
    if (!eventsStarted) throw new IllegalStateException
    _eventWritten = true
    statistics.countWillBeCommittedEvents(stampedEvents.size)
    for (stamped ← stampedEvents) {
      if (stamped.eventId <= _lastEventId)
        throw new IllegalArgumentException(s"JournalWriter.writeEvent with EventId ${EventId.toString(stamped.eventId)} <= lastEventId ${EventId.toString(_lastEventId)}")
      _lastEventId = stamped.eventId
      val byteString =
        try ByteString(stamped.asJson.compactPrint)
        catch { case t: Exception ⇒ throw new SerializationException(t) }
      jsonWriter.write(byteString)
      for (_ ← eventReader) positionsAndEventIds += PositionAnd(jsonWriter.fileLength, stamped.eventId)
    }
  }

  def flush(): Unit =
    if (!isFlushed) {
      statistics.beforeFlush()
      jsonWriter.flush()
      statistics.afterFlush()
      for (e ← eventReader) {
        for (o ← positionsAndEventIds) e.onEventAdded(flushedPosition = o.position, eventId = o.value)
        if (positionsAndEventIds.size <= PositionBufferMaxSize) {
          positionsAndEventIds.clear()
        } else {
          positionsAndEventIds = mutable.Buffer[PositionAnd[EventId]]()
        }
      }
    }

  def sync(): Unit =
    if (!isSynced) {
      statistics.beforeSync()
      jsonWriter.sync()
      statistics.afterSync()
    }

  def isFlushed = jsonWriter.isFlushed

  def isSynced = jsonWriter.isSynced

  def lastEventId = _lastEventId

  def isEventWritten = _eventWritten
}

private[journal] object JournalWriter {
  val SnapshotsHeader = Json.fromString("-------SNAPSHOTS-------")
  val EventsHeader    = Json.fromString("-------EVENTS-------")
  private val PositionBufferMaxSize = 10000
  private val logger = Logger(getClass)

  final class SerializationException(cause: Throwable) extends RuntimeException("JSON serialization error", cause)
}
