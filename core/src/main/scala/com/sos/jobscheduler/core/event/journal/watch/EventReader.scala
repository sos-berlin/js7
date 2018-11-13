package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalReader
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.typesafe.config.Config
import java.nio.file.Path
import monix.execution.atomic.AtomicAny

/**
  * @author Joacim Zschimmer
  */
private[watch] trait EventReader[E <: Event]
extends AutoCloseable
{
  /** `flushedLength` does not grow if `isOwnJournalIndex`. */
  protected val journalMeta: JournalMeta[E]
  protected def isHistoric: Boolean
  protected def journalFile: Path
  protected def tornEventId: EventId
  protected def tornPosition: Long
  /** Must be constant if `isHistoric`. */
  protected def flushedLength: Long
  protected def config: Config

  private lazy val logger = Logger.withPrefix[EventReader[E]](journalFile.getFileName.toString)
  protected lazy val journalIndex = new JournalIndex(PositionAnd(tornPosition, tornEventId),
    size = config.getInt("jobscheduler.journal.watch.index-size"))
  private lazy val journalIndexFactor = config.getInt("jobscheduler.journal.watch.index-factor")
  protected final lazy val iteratorPool = new FileEventIteratorPool(journalMeta, journalFile, tornEventId, () ⇒ flushedLength)
  @volatile
  private var _closeAfterUse = false
  @volatile
  private var _lastUsed = 0L

  final def closeAfterUse(): Unit = {
    logger.debug("closeAfterUse")
    _closeAfterUse = true
    if (!isInUse) close()
  }

  final def close(): Unit =
    iteratorPool.close()

  /**
    * @return None if torn
    */
  final def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]] = {
    val indexPositionAndEventId = journalIndex.positionAndEventIdAfter(after)
    import indexPositionAndEventId.position
    val iteratorAtomic = AtomicAny(iteratorPool.borrowIterator())
    def iterator = iteratorAtomic.get match {
      case null ⇒ throw new IllegalStateException("FileEventIterator closed")
      case o ⇒ o
    }
    def returnIterator(): Unit =
      iteratorAtomic.getAndSet(null) match {
        case null ⇒
        case o ⇒ iteratorPool.returnIterator(o)
      }
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
        returnIterator()
        None
      } else
        Some(
          new CloseableIterator[Stamped[KeyedEvent[E]]] {
            @volatile private var eof = false
            @volatile var closed = false

            // May be called asynchronously (parallel to hasNext or next), as by Monix doOnSubscriptionCancel
            def close() =
              synchronized {
                if (!closed) {
                  closed = true
                  returnIterator()
                  if (_closeAfterUse && !isInUse || iteratorPool.isClosed) {
                    logger.debug(s"CloseableIterator.close _closeAfterUse: '${EventReader.this}'")
                    EventReader.this.close()
                  }
                }
              }

            def hasNext =
              synchronized {
                !eof && {  // Avoid exception in iterator in case of automatically closed iterator (closeAtEnd, for testing)
                  requireNotClosed()
                  val has = iterator.hasNext
                  eof |= !has
                  if (!has && isHistoric) {
                    journalIndex.freeze(journalIndexFactor)
                  }
                  has
                }
              }

            def next() =
              synchronized {
                requireNotClosed()
                _lastUsed = Timestamp.currentTimeMillis
                val stamped = iterator.next()
                assert(stamped.eventId >= after, s"${stamped.eventId} ≥ $after")
                if (isHistoric) {
                  if (eof/*freezed*/) sys.error(s"FileEventIterator: !hasNext but next() returns a value, eventId=${stamped.eventId} position=${iterator.position}")
                   journalIndex.tryAddAfter(stamped.eventId, iterator.position)
                }
                stamped
              }

            private def requireNotClosed() =
              if (closed) throw new IllegalStateException("FileEventIterator has been closed")
          }.closeAtEnd)
    }
  }

  final def snapshotObjects: CloseableIterator[Any] =
    CloseableIterator.fromCloseable(new JournalReader(journalMeta, journalFile))(_.nextSnapshots())

  final def lastUsedAt: Long =
    _lastUsed

  final def isInUse = iteratorPool.isLent
}
