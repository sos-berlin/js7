package js7.journal.watch

import com.typesafe.config.Config
import js7.base.monixutils.MonixDeadline
import js7.base.utils.CloseableIterator
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalId}
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles.JournalMetaOps

/**
  * @author Joacim Zschimmer
  */
private[watch] final class CurrentEventReader(
  protected val journalMeta: JournalMeta,
  protected val expectedJournalId: JournalId,
  /** Length and after-EventId of initialized and empty journal. */
  tornLengthAndEventId: PositionAnd[EventId],
  flushedLengthAndEventId: PositionAnd[EventId],
  val isActiveNode: Boolean,
  protected val config: Config)
extends EventReader
{
  protected def isHistoric = false
  protected def tornPosition = tornLengthAndEventId.position
  def tornEventId = tornLengthAndEventId.value
  val journalFile = journalMeta.file(after = tornEventId)

  /** May contain size(file) + 1 to allow EOF detection. */
  private val flushedLengthSync = new EventSync(initial = 0, o => s"position $o")
  flushedLengthSync.onAdded(flushedLengthAndEventId.position)

  @volatile private var journalingEnded = false
  @volatile private var _committedLength = flushedLengthAndEventId.position
  @volatile private var _lastEventId = flushedLengthAndEventId.value

  protected[journal] def committedLength = _committedLength

  protected def isEOF(position: Long) = journalingEnded && position >= _committedLength

  protected def whenDataAvailableAfterPosition(position: Long, until: MonixDeadline) =
    flushedLengthSync.whenAvailable(position, until = Some(until))

  protected def isFlushedAfterPosition(position: Long) =
    journalingEnded || position < flushedLengthSync.last

  private[journal] def onJournalingEnded(fileLength: Long) = {
    flushedLengthSync.onAdded(fileLength + 1)  // Plus one, to allow EOF detection
    _committedLength = fileLength
    journalingEnded = true
  }

  private[journal] def onFileWritten(flushedPosition: Long): Unit =
    if (flushedPosition > flushedLengthSync.last) {
      flushedLengthSync.onAdded(flushedPosition)
    }

  private[journal] def onEventsCommitted(positionAndEventId: PositionAnd[EventId], n: Int): Unit = {
    val PositionAnd(pos, eventId) = positionAndEventId
    journalIndex.addAfter(eventId = eventId, position = pos, n = n)
    _committedLength = pos
    _lastEventId = eventId
  }

  protected def reverseEventsAfter(after: EventId) =
    CloseableIterator.empty  // Not implemented

  def lastEventId = _lastEventId

  def isEnded = journalingEnded

  override def toString = s"CurrentEventReader(${journalFile.getFileName})"
}
