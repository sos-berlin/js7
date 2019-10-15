package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.event.{EventSync, PositionAnd}
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops.RichTaskCompanion
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId}
import com.typesafe.config.Config
import monix.eval.Task
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
private[watch] final class CurrentEventReader[E <: Event](
  protected val journalMeta: JournalMeta[E],
  protected val expectedJournalId: Option[JournalId],
  /** Length and after-EventId of initialized and empty journal. */
  flushedLengthAndEventId: PositionAnd[EventId],
  protected val config: Config)
extends EventReader[E]
{
  protected def isHistoric = false
  val tornEventId = flushedLengthAndEventId.value
  val journalFile = journalMeta.file(after = tornEventId)

  /** May contain size(file) + 1 to allow EOF detection. */
  private val flushedLengthSync = new EventSync(initial = 0, o => s"position $o")
  @volatile private var journalingEnded = false
  @volatile private var _committedLength = flushedLengthAndEventId.position
  @volatile private var _lastEventId = flushedLengthAndEventId.value

  // Initially, the file contains no events but a JournalHeader
  flushedLengthSync.onAdded(flushedLengthAndEventId.position)

  protected def tornPosition = flushedLengthAndEventId.position

  protected[journal] def committedLength = _committedLength

  protected def whenDataAvailableAfterPosition(position: Long, until: Deadline) =
    if (isFlushedAfterPosition(position))
      Task.True
    else
      flushedLengthSync.whenAvailable(position, until = Some(until))

  protected def isFlushedAfterPosition(position: Long) =
    journalingEnded || position < flushedLengthSync.last

  protected def isEOF(position: Long) =
    journalingEnded && position >= committedLength

  private[journal] def onJournalingEnded(fileLength: EventId) = {
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

  override def toString = s"CurrentEventReader(${journalFile.getFileName})"
}
