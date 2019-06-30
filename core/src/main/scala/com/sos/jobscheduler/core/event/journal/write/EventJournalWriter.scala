package com.sos.jobscheduler.core.event.journal.write

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators.{Commit, EventFooter, EventHeader, Transaction}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles._
import com.sos.jobscheduler.core.event.journal.watch.JournalingObserver
import com.sos.jobscheduler.core.event.journal.write.EventJournalWriter._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import io.circe.syntax.EncoderOps
import java.nio.file.Path
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
private[journal] final class EventJournalWriter[E <: Event](
  journalMeta: JournalMeta[E],
  val file: Path,
  after: EventId,
  observer: Option[JournalingObserver],
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

  def closeProperly(sync: Boolean): Unit =
    try if (eventsStarted) endEventSection(sync = sync)
    finally close()

  override def close(): Unit = {
    super.close()
    for (o <- statistics.debugString) logger.debug(o)
  }

  def beginEventSection(): Unit = {
    if (eventsStarted) throw new IllegalStateException("EventJournalWriter: duplicate beginEventSection()")
    jsonWriter.write(ByteString(EventHeader.compactPrint))
    flush(sync = false)
    eventsStarted = true
    for (r <- observer) {
      r.onJournalingStarted(file, PositionAnd(jsonWriter.fileLength, _lastEventId))
    }
  }

  def writeEvents(stampedEvents: Seq[Stamped[KeyedEvent[E]]], transaction: Boolean = false): Unit = {
    // TODO Rollback writes in case of error (with seek?)
    if (!eventsStarted) throw new IllegalStateException
    _eventWritten = true
    statistics.countEventsToBeCommitted(stampedEvents.size)
    val ta = transaction && stampedEvents.lengthCompare(1) > 0
    if (ta) jsonWriter.write(TransactionByteString)
    for (stamped <- stampedEvents) {
      if (stamped.eventId <= _lastEventId)
        throw new IllegalArgumentException(s"EventJournalWriter.writeEvent with EventId ${EventId.toString(stamped.eventId)} <= lastEventId ${EventId.toString(_lastEventId)}")
      _lastEventId = stamped.eventId
      val byteString =
        try ByteString(stamped.asJson.compactPrint)
        catch { case t: Exception => throw new SerializationException(t) }
      jsonWriter.write(byteString)
      notFlushedCount += stampedEvents.length
    }
    if (ta) jsonWriter.write(CommitByteString)
  }

  def endEventSection(sync: Boolean): Unit = {
    if (!eventsStarted) throw new IllegalStateException
    jsonWriter.write(ByteString(EventFooter.compactPrint))
    flush(sync = sync)
    logger.debug(s"Journal finished, $fileSizeString written ($statistics)")
  }

  override def flush(sync: Boolean): Unit = {
    super.flush(sync)
    // TODO Notify observer first after sync! OrderStdWritten braucht dann und wann ein sync (1s), um observer nicht lange warten zu lassen.
    if (notFlushedCount > 0) for (r <- observer) {
      r.onEventsAdded(PositionAnd(jsonWriter.fileLength, _lastEventId), n = notFlushedCount)
      notFlushedCount = 0
    }
  }

  def isEventWritten = _eventWritten

  override def toString = s"EventJournalWriter(${file.getFileName})"
}

private[journal] object EventJournalWriter
{
  private val TransactionByteString = ByteString(Transaction.asJson.compactPrint)
  private val CommitByteString = ByteString(Commit.asJson.compactPrint)

  def forTest[E <: Event](journalMeta: JournalMeta[E], after: EventId,
    observer: Option[JournalingObserver] = None, withoutSnapshots: Boolean = true)
  =
    new EventJournalWriter[E](journalMeta, journalMeta.file(after), after, observer, simulateSync = None, withoutSnapshots = withoutSnapshots)

  final class SerializationException(cause: Throwable) extends RuntimeException("JSON serialization error", cause)
}
