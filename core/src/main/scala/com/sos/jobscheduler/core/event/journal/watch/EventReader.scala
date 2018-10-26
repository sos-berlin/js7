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
  protected def reusableJournalIndex: Option[JournalIndex]
  protected def config: Config

  private lazy val logger = Logger.withPrefix[EventReader[E]](journalFile.getFileName.toString)
  protected lazy val journalIndex = reusableJournalIndex getOrElse
    new JournalIndex(PositionAnd(tornPosition, tornEventId), size = config.getInt("jobscheduler.journal.watch.index-size"))
  protected final lazy val iteratorPool = new FileEventIteratorPool(journalMeta, journalFile, tornEventId, () ⇒ flushedLength)
  @volatile
  private var _closeAfterUse = false
  @volatile
  private var _lastUsed = 0L

  final def closeAfterUse(): Unit = {
    _closeAfterUse = true
    if (!isInUse) close()
  }

  final def close(): Unit =
    iteratorPool.close()

  final def freeze(): Unit =
    if (!journalIndex.isFreezed) {
      journalIndex.freeze(config.getInt("jobscheduler.journal.watch.index-factor"))
    }

  /**
    * @return None if torn
    */
  final def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]] = {
    val indexPositionAndEventId = journalIndex.positionAndEventIdAfter(after)
    import indexPositionAndEventId.position
    if (position >= flushedLength)  // Data behind flushedLength is not flushed and probably incomplete
      Some(CloseableIterator.empty)
    else {
      val iterator = iteratorPool.borrowIterator()
      closeOnError(iterator) {
        if (iterator.position != position &&
          (iterator.position < position || iterator.eventId > after/*No seek if skipToEventAfter works without seek*/))
        {
          logger.trace(s"seek $position (eventId=${indexPositionAndEventId.value}, for $after) ≠ " +
            s"iterator ${iterator.position} (eventId=${iterator.eventId})")
          iterator.seek(indexPositionAndEventId)
        }
        val exists = iterator.skipToEventAfter(journalIndex, after) // May run very long (minutes for gigabyte journals) !!!
        if (!exists) {
          iteratorPool.returnIterator(iterator)
          None
        } else
          Some(
            new CloseableIterator[Stamped[KeyedEvent[E]]] {
              var closed = false

              def close() = if (!closed) {
                closed = true
                iteratorPool.returnIterator(iterator)
                if (_closeAfterUse && !isInUse || iteratorPool.isClosed) {
                  logger.debug(s"CloseableIterator.close _closeAfterUse: '${EventReader.this}'")
                  EventReader.this.close()
                }
              }

              def hasNext = {
                val r = iterator.hasNext
                if (!r && isHistoric) freeze()
                r
              }

              def next() = {
                _lastUsed = Timestamp.currentTimeMillis
                val stamped = iterator.next()
                assert(stamped.eventId >= after, s"${stamped.eventId} ≥ $after")
                if (isHistoric) {
                  journalIndex.tryAddAfter(stamped.eventId, iterator.position)
                }

                stamped
              }
            }.closeAtEnd)
      }
    }
  }

  final def lastUsedAt: Long =
    _lastUsed

  final def isInUse = iteratorPool.isLent
}
