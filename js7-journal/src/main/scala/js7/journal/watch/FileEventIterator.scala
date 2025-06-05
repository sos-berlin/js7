package js7.journal.watch

import java.nio.file.Path
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.metering.CallMeter
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CloseableIterator
import js7.common.jsonseq.PositionAnd
import js7.data.event.{Event, EventId, JournalId, KeyedEvent, SnapshotableState, Stamped}
import js7.journal.recover.JournalReader
import js7.journal.watch.FileEventIterator.*
import scala.concurrent.duration.Deadline.now
import scala.language.unsafeNulls

/**
  * @author Joacim Zschimmer
  */
private[watch] class FileEventIterator(
  S: SnapshotableState.HasCodec,
  val journalFile: Path,
  expectedJournalId: JournalId,
  fileEventId: EventId,
  committedLength: () => Long)
extends CloseableIterator[Stamped[KeyedEvent[Event]]]:
  private val logger = Logger.withPrefix[this.type](journalFile.getFileName.toString)
  private val journalReader = new JournalReader(S, journalFile, expectedJournalId)
  private var nextEvent: Stamped[KeyedEvent[Event]] = null
  private var closed = false

  closeOnError(journalReader):
    if journalReader.fileEventId != fileEventId then sys.error(
      s"Journal file '$journalFile': found fileEventId=${journalReader.fileEventId}, " +
        s"expected was: $fileEventId")

  def close(): Unit =
    if !closed then
      closed = true
      journalReader.close()

  final def firstEventPosition = journalReader.firstEventPosition

  final def seek(positionAndEventId: PositionAnd[EventId]): Unit =
    journalReader.seekEvent(positionAndEventId)
    nextEvent = null

  /** May take minutes for a gigabygte journal..
    * @return false iff `after` is unknown
    */
  final def skipToEventAfter(journalIndex: JournalIndex, after: EventId): Boolean =
    eventId <= after && (
      eventId == after ||
        // After timeout a client may try again. We synchronize these probably
        // idempotent calls (multiple FileEventIterators share the JournalIndex)
        meterSkip:
          journalIndex.synchronizeBuilding:
            // May take a long time !!!
            val watch = TimeWatch(after)
            while eventId < after && hasNext do
              next()
              val PositionAnd(position, eventId) = positionAndEventId
              journalIndex.tryAddAfter(eventId, position)
              watch.onSkipped()
            watch.end()
            eventId == after
      )

  final def hasNext = nextEvent != null ||
    journalReader.position < committedLength() && {
      nextEvent = journalReader.nextEvent().orNull
      nextEvent != null
    }

  final def next(): Stamped[KeyedEvent[Event]] =
    hasNext
    val r = nextEvent
    if r == null then throw new NoSuchElementException(
      s"End of committed part of journal file '${journalFile.getFileName}' reached")
    nextEvent = null
    r

  final def eventId = journalReader.eventId
  final def position = journalReader.position
  final def positionAndEventId = journalReader.positionAndEventId
  final def isClosed = closed

  override def toString =
    s"FileEventIterator(${journalFile.getFileName} fileEventId=${EventId.toString(fileEventId)} " +
      s"eventId=$eventId)"

  private class TimeWatch(after: EventId):
    private val PositionAnd(startPosition, startEventId) = positionAndEventId
    private val runningSince = now
    private var skipped = 0
    private var debugIssued = false

    def onSkipped(): Unit =
      skipped += 1
      val duration = runningSince.elapsed
      if duration >= LogThreshold &&
        !debugIssued && (position - startPosition >= InfoSkippedSize || duration >= 1.s)
      then
        logger.debug:
          s"⏳ $skipped events (${toKBGB(position - startPosition)
          }) skipped since ${duration.pretty} ago: ${EventId.toDateTimeString(startEventId)
          }..${EventId.toDateTimeString(after)} in journal file"
        debugIssued = true

    def end(): Unit =
      val duration = runningSince.elapsed
      if duration >= LogThreshold then
        val skippedSize = position - startPosition
        val logLevel = if skipped == 0 then LogLevel.None
          else if skippedSize >= InfoSkippedSize || duration >= InfoDuration then LogLevel.Info
          else LogLevel.Trace
        logger.log(logLevel, s"⏳ $skipped events (${toKBGB(skippedSize)}) skipped in ${
          duration.pretty}: ${EventId.toString(startEventId)}..${EventId.toString(after)}")


object FileEventIterator:
  private val InfoSkippedSize = 10*1000*1000
  private val InfoDuration = 3.s
  private val meterSkip = CallMeter("FileEventIterator.skipToEventAfter")
  private val LogThreshold = 500.µs
