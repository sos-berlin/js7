package js7.journal.watch

import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import js7.base.catsutils.CatsDeadline
import js7.base.stream.IncreasingNumberSync
import js7.base.utils.CloseableIterator
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalId, JournalPosition}
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.JournalMetaOps

/**
  * @author Joacim Zschimmer
  */
private[watch] final class CurrentEventReader(
  protected val journalLocation: JournalLocation,
  protected val expectedJournalId: JournalId,
  /** Length and after-EventId of initialized and empty journal. */
  firstEventPositionAndFileEventId: PositionAnd[EventId],
  flushedLengthAndEventId: PositionAnd[EventId],
  val isActiveNode: Boolean,
  protected val config: Config,
  protected val ioRuntime: IORuntime)
extends EventReader:
  protected def isHistoric = false
  protected def firstEventPosition = firstEventPositionAndFileEventId.position
  def fileEventId = firstEventPositionAndFileEventId.value
  val journalFile = journalLocation.file(after = fileEventId)

  /** May contain size(file) + 1 to allow EOF detection. */
  private val flushedLengthSync = new IncreasingNumberSync(initial = 0, o => s"position $o")
  flushedLengthSync.onAdded(flushedLengthAndEventId.position)

  @volatile private var journalingEnded = false
  @volatile private var _committedLength = flushedLengthAndEventId.position
  @volatile private var _lastEventId = flushedLengthAndEventId.value

  protected def committedLength = _committedLength

  protected[journal] def journalPosition: JournalPosition =
    synchronized:
      JournalPosition(fileEventId, _committedLength)

  protected def isEOF(position: Long) =
    synchronized:
      journalingEnded && position >= _committedLength

  protected def whenDataAvailableAfterPosition(position: Long, until: CatsDeadline) =
    flushedLengthSync.whenAvailable(position, until = Some(until))

  protected def isFlushedAfterPosition(position: Long) =
    synchronized:
      journalingEnded || position < flushedLengthSync.last

  private[journal] def onJournalingEnded(fileLength: Long): Unit =
    synchronized:
      flushedLengthSync.onAdded(fileLength + 1)  // Plus one, to allow EOF detection
      _committedLength = fileLength
      journalingEnded = true

  private[journal] def onFileWritten(flushedPosition: Long): Unit =
    if flushedPosition > flushedLengthSync.last then
      flushedLengthSync.onAdded(flushedPosition)

  private[journal] def onEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit =
    synchronized:
      val PositionAnd(pos, eventId) = positionAndEventId
      journalIndex.addAfter(eventId = eventId, position = pos, n = n)
      _committedLength = pos
      _lastEventId = eventId

  protected def reverseEventsAfter(after: EventId) =
    CloseableIterator.empty  // Not implemented

  def lastEventId = _lastEventId

  def isEnded = journalingEnded

  override def toString = s"CurrentEventReader(${journalFile.getFileName})"
