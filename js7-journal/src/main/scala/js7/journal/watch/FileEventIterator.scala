package js7.journal.watch

import java.nio.file.Path
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.CloseableIterator
import js7.common.jsonseq.PositionAnd
import js7.common.scalautil.Logger
import js7.common.utils.ByteUnits.toKBGB
import js7.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import js7.journal.data.JournalMeta
import js7.journal.recover.JournalReader
import js7.journal.watch.FileEventIterator._
import scala.concurrent.blocking
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
private[watch] class FileEventIterator(
  journalMeta: JournalMeta,
  val journalFile: Path,
  expectedJournalId: Option[JournalId],
  tornEventId: EventId,
  committedLength: () => Long)
extends CloseableIterator[Stamped[KeyedEvent[Event]]]
{
  private val logger = Logger.withPrefix[this.type](journalFile.getFileName.toString)
  private val journalReader = new JournalReader(journalMeta, expectedJournalId, journalFile)
  private var nextEvent: Stamped[KeyedEvent[Event]] = null
  private var closed = false

  closeOnError(journalReader) {
    if (journalReader.tornEventId != tornEventId) sys.error(s"Journal file '$journalFile': found eventId=${journalReader.tornEventId}, expected was: $tornEventId")
  }

  def close(): Unit =
    if (!closed) {
      closed = true
      journalReader.close()
    }

  final def firstEventPosition = journalReader.firstEventPosition

  final def seek(positionAndEventId: PositionAnd[EventId]): Unit = {
    journalReader.seekEvent(positionAndEventId)
    nextEvent = null
  }

  /** May take minutes for a gigabygte journal..
    * @return false iff `after` is unknown
    */
  final def skipToEventAfter(journalIndex: JournalIndex, after: EventId): Boolean =
    eventId <= after &&
      (eventId == after ||
        journalIndex.synchronizeBuilding {  // After timeout a client may try again. We synchronize these probably idempotent calls (multiple FileEventIterators share the JournalIndex)
          blocking {  // May take a long time !!!
            val watch = new TimeWatch(after)
            while (eventId < after) {
              if (!hasNext) return false
              next()
              val PositionAnd(position, eventId) = positionAndEventId
              journalIndex.tryAddAfter(eventId, position)
              watch.onSkipped()
            }
            watch.end()
            eventId == after
          }
        })

  final def hasNext = nextEvent != null ||
    journalReader.position < committedLength() && {
      nextEvent = journalReader.nextEvent().orNull
      nextEvent != null
    }

  final def next(): Stamped[KeyedEvent[Event]] = {
    hasNext
    val r = nextEvent
    if (r == null) throw new NoSuchElementException(
      s"End of committed part of journal file '${journalFile.getFileName}' reached")
    nextEvent = null
    r
  }

  final def eventId = journalReader.eventId
  final def position = journalReader.position
  final def positionAndEventId = journalReader.positionAndEventId
  final def isClosed = closed

  override def toString =
    s"FileEventIterator(${journalFile.getFileName} tornEventId=${EventId.toString(tornEventId)} eventId=$eventId)"

  private class TimeWatch(after: EventId) {
    private val PositionAnd(startPosition, startEventId) = positionAndEventId
    private val runningSince = now
    private var skipped = 0
    private var debugIssued = false

    def onSkipped(): Unit = {
      skipped += 1
      val duration = runningSince.elapsed
      def msg = s"$skipped events (${toKBGB(position - startPosition)}) skipped since ${duration.pretty}" +
        s" while searching ${EventId.toDateTimeString(startEventId)}..${EventId.toDateTimeString(after)} in journal, "
      if (!debugIssued && (position - startPosition >= 100*1000*1000 || duration > 10.s)) {
        logger.debug(msg)
        debugIssued = true
      }
    }

    def end(): Unit = {
      val skippedSize = position - startPosition
      val duration = runningSince.elapsed
      if (skipped > 0)
        logger.trace(s"$skipped events (${toKBGB(skippedSize)}) skipped in ${duration.pretty} for searching ${EventId.toString(startEventId)}..${EventId.toString(after)}")
      if (skippedSize >= WarnSkippedSize || duration >= WarnDuration)
        logger.info(s"$skipped events (${toKBGB(skippedSize)}) read in ${duration.pretty}, in search of event '${EventId.toString(startEventId)}'")
    }
  }
}

object FileEventIterator {
  private val WarnSkippedSize = 100*1000*100
  private val WarnDuration = 30.s
}
