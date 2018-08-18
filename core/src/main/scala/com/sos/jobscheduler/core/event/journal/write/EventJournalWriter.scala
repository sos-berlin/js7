package com.sos.jobscheduler.core.event.journal.write

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalHeaders.EventsHeader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles._
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.write.EventJournalWriter.SerializationException
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import io.circe.syntax.EncoderOps
import java.nio.file.{Files, Path}
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
private[journal] final class EventJournalWriter[E <: Event](
  journalMeta: JournalMeta[E],
  val file: Path,
  after: EventId,
  protected val observer: Option[JournalingObserver],
  protected val simulateSync: Option[FiniteDuration],
  withoutSnapshots: Boolean = false)
extends JournalWriter[E](append = !withoutSnapshots)
with AutoCloseable
{
  import journalMeta.eventJsonCodec

  private val logger = Logger.withPrefix(getClass, file.getFileName.toString)
  protected val statistics = new EventStatisticsCounter
  private var _lastEventId = after
  private var eventsStarted = false
  private var _eventWritten = false
  private var notFlushedCount = 0

  if (!Files.exists(file)) sys.error(s"EventJournalWriter: Not expecting existent files '$file'")

  override def close(): Unit = {
    super.close()
    logger.info(s"Journal closed, $fileSizeString written ($statistics)")
    for (o ← statistics.debugString) logger.debug(o)
  }

  def startJournaling(): Unit = {
    if (eventsStarted) throw new IllegalStateException("EventJournalWriter: duplicate startJournaling()")
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
        throw new IllegalArgumentException(s"EventJournalWriter.writeEvent with EventId ${EventId.toString(stamped.eventId)} <= lastEventId ${EventId.toString(_lastEventId)}")
      _lastEventId = stamped.eventId
      val byteString =
        try ByteString(stamped.asJson.compactPrint)
        catch { case t: Exception ⇒ throw new SerializationException(t) }
      jsonWriter.write(byteString)
      notFlushedCount += stampedEvents.length
    }
  }

  override def flush(sync: Boolean): Unit = {
    super.flush(sync)
    if (notFlushedCount > 0) for (r ← observer) {
      notFlushedCount = 0
      r.onEventsAdded(PositionAnd(jsonWriter.fileLength, _lastEventId), n = notFlushedCount)
    }
  }

  def isEventWritten = _eventWritten

  override def toString = s"EventJournalWriter(${file.getFileName})"
}

private[journal] object EventJournalWriter
{
  def forTest[E <: Event](journalMeta: JournalMeta[E], after: EventId, observer: Option[JournalingObserver] = None) =
    new EventJournalWriter[E](journalMeta, journalMeta.file(after), after, observer, simulateSync = None, withoutSnapshots = true)

  final class SerializationException(cause: Throwable) extends RuntimeException("JSON serialization error", cause)
}
