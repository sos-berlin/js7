package com.sos.jobscheduler.core.event.journal.write

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.ByteUnits.toMB
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalHeaders.{EventsHeader, SnapshotsHeader}
import com.sos.jobscheduler.core.event.journal.data.{JournalHeader, JournalMeta, SnapshotMeta}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.write.JournalWriter.SerializationException
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import io.circe.syntax.EncoderOps
import java.nio.file.{Files, Path}
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[journal] final class JournalWriter[E <: Event](
  journalMeta: JournalMeta[E],
  val file: Path,
  after: EventId,
  observer: Option[JournalingObserver],
  appendToSnapshots: Boolean,
  simulateSync: Option[FiniteDuration])
extends AutoCloseable
{
  def this(journalMeta: JournalMeta[E], after: EventId, observer: Option[JournalingObserver] = None, appendToSnapshots: Boolean = false) =
    this(journalMeta, journalMeta.file(after), after, observer, appendToSnapshots, None)

  import journalMeta.eventJsonCodec

  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)

  if (!appendToSnapshots && Files.exists(file)) sys.error(s"JournalWriter: Not expecting existent files '$file'")
  if (appendToSnapshots && !Files.exists(file)) sys.error(s"JournalWriter: Missing files '$file'")

  private val jsonWriter = new FileJsonWriter(JournalHeader.Singleton, file, append = appendToSnapshots, simulateSync = simulateSync)
  private var snapshotStarted = appendToSnapshots
  private var eventsStarted = false
  private var _eventWritten = false
  private var _lastEventId = after
  private var eventAdded = false
  private val statistics = new StatisticsCounter

  def close() = {
    jsonWriter.close()
    logger.info((try toMB(Files.size(file)) catch { case NonFatal(t) ⇒ t.toString }) + s" written: ${statistics.infoString}")
    logTiming()
  }

  def logTiming(): Unit =
    logger.debug(statistics.debugString + " " + statistics.timingString)

  def beginSnapshotSection(): Unit = {
    if (snapshotStarted) throw new IllegalStateException("JournalWriter: duplicate beginSnapshotSection()")
    logger.info("Snapshot starts")
    jsonWriter.write(ByteString(SnapshotsHeader.compactPrint))
    jsonWriter.write(ByteString(SnapshotMeta(_lastEventId).asJson.compactPrint))
    flush(sync = false)
    snapshotStarted = true
  }

  def writeSnapshot(json: ByteString): Unit = {
    if (!snapshotStarted) throw new IllegalStateException("JournalWriter: writeSnapshots(), but snapshots have not been started")
    if (eventsStarted) throw new IllegalStateException("JournalWriter: writeSnapshots(), but events have already been started")
    jsonWriter.write(json)
  }

  def startJournaling(): Unit = {
    if (!snapshotStarted) throw new IllegalStateException("JournalWriter: beginSnapshotSection not called")
    if (eventsStarted) throw new IllegalStateException("JournalWriter: duplicate startJournaling()")
    jsonWriter.write(ByteString(EventsHeader.compactPrint))
    flush(sync = false)
    eventsStarted = true
    for (r ← observer) {
      r.onJournalingStarted(file, PositionAnd(jsonWriter.fileLength, _lastEventId))
    }
  }

  def writeEvents(stampedEvents: Seq[Stamped[KeyedEvent[E]]]): Unit = {
    if (!eventsStarted) throw new IllegalStateException
    _eventWritten = true
    statistics.countEventsToBeCommitted(stampedEvents.size)
    for (stamped ← stampedEvents) {
      if (stamped.eventId <= _lastEventId)
        throw new IllegalArgumentException(s"JournalWriter.writeEvent with EventId ${EventId.toString(stamped.eventId)} <= lastEventId ${EventId.toString(_lastEventId)}")
      _lastEventId = stamped.eventId
      val byteString =
        try ByteString(stamped.asJson.compactPrint)
        catch { case t: Exception ⇒ throw new SerializationException(t) }
      jsonWriter.write(byteString)
      eventAdded = true
    }
  }

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
    if (eventAdded) for (r ← observer) {
      eventAdded = false
      r.onEventsAdded(PositionAnd(jsonWriter.fileLength, _lastEventId))
    }
  }

  def isFlushed = jsonWriter.isFlushed

  def isSynced = jsonWriter.isSynced

  def isEventWritten = _eventWritten

  override def toString = s"JournalWriter(${file.getFileName})"
}

private[journal] object JournalWriter {
  final class SerializationException(cause: Throwable) extends RuntimeException("JSON serialization error", cause)
}
