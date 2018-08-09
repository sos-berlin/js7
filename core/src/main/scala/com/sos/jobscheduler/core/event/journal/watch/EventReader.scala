package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.typesafe.config.Config
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[watch] trait EventReader[E <: Event]
extends AutoCloseable
{
  /** `flushedLength` does not grow if `isHistoric`. */
  protected def isHistoric: Boolean
  protected val journalMeta: JournalMeta[E]
  protected def journalFile: Path
  protected def tornEventId: EventId
  protected def tornPosition: Long
  /** Must be constant if `isHistoric`. */
  protected def flushedLength: Long
  protected def config: Config

  private lazy val logger = Logger.withPrefix[EventReader[E]](journalFile.getFileName.toString)
  protected lazy val eventIdToPositionIndex = new EventIdPositionIndex(size = config.getInt("jobscheduler.journal.watch.index-size"))
  protected lazy val iteratorPool = new FileEventIteratorPool(journalMeta, journalFile, tornEventId, () ⇒ flushedLength)

  eventIdToPositionIndex.addAfter(tornEventId, tornPosition)

  def close() = iteratorPool.close()

  final def eventsAfter(after: EventId): CloseableIterator[Stamped[KeyedEvent[E]]] = {
    val PositionAnd(position, positionEventId) = eventIdToPositionIndex.positionAndEventIdAfter(after)
    if (position >= flushedLength)  // Data behind flushedLength is not flushed and probably incomplete
      CloseableIterator.empty
    else {
      val iterator = iteratorPool.borrowIterator()
      closeOnError(iterator) {
        if (iterator.position != position &&
          (iterator.position < position || iterator.eventId.forall(_ > after)/*No seek if skipToEvensAfter works without seek*/))
        {
            logger.trace(s"seek $position (eventId=$positionEventId, for $after) ≠ iterator ${iterator.position} (eventId=${iterator.eventId getOrElse "-"})")
            iterator.seek(position)
        }
        iterator.skipToEventAfter(after)

        new CloseableIterator[Stamped[KeyedEvent[E]]] {
          var closed = false

          def close() = if (!closed) {
            closed = true
            iteratorPool.returnIterator(iterator)
          }

          def hasNext = iterator.hasNext

          def next() = {
            val stamped = iterator.next()
            assert(stamped.eventId >= after, s"${stamped.eventId} ≥ $after")
            stamped
          }
        }.closeAtEnd
      }
    }
  }

  final def lastUsedAt: Timestamp =
    iteratorPool.lastUsedAt

  final def isInUse = iteratorPool.isLent
}
