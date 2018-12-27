package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.FileEventIteratorPool._
import com.sos.jobscheduler.data.event.{Event, EventId}
import java.nio.file.Path
import monix.execution.atomic.AtomicBoolean
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[watch] final class FileEventIteratorPool[E <: Event](journalMeta: JournalMeta[E], journalFile: Path, tornEventId: EventId,
  flushedLength: () ⇒ Long)
{
  private val freeIterators = mutable.ArrayBuffer[FileEventIterator[E]]()
  private val lentIterators = mutable.ArrayBuffer[FileEventIterator[E]]()
  private val closed = AtomicBoolean(false)

  def close(): Unit =
    synchronized {
      if (!closed.getAndSet(true)) {
        val (availables, lent): (Set[FileEventIterator[E]], Set[FileEventIterator[E]]) =
          synchronized {
            (freeIterators.toSet, lentIterators.toSet)
          }
        if (lent.nonEmpty) {
          logger.info(s"Close '$toString' while ${lent.size}× opened")  // May abort open web requests
        }
        (availables ++ lent) foreach (_.close())  // Force close iterators, for Windows to unlock the file - required for testing
      }
    }

  lazy val firstEventPosition: Long = {
    val iterator = borrowIterator()
    try iterator.firstEventPosition
    catch { case NonFatal(t) ⇒
      iterator.close()
      throw t
    }
    finally returnIterator(iterator)   // JsonSeqReader is positioned after tornEventId
  }

  def borrowIterator(): FileEventIterator[E] = {
    if (closed()) throw new ClosedException(journalFile)
    tryBorrowIterator() getOrElse newIterator()
  }

  private def tryBorrowIterator(): Option[FileEventIterator[E]] =
    synchronized {
      freeIterators.nonEmpty ? {
        val iterator = freeIterators.remove(freeIterators.size - 1)  // LIFO
        lentIterators += iterator
        logger.trace(s"borrowIterator $iterator eventId=${iterator.eventId} position=${iterator.position}")
        iterator
      }
    }

  private def newIterator(): FileEventIterator[E] =
    synchronized {
      if (closed()) throw new ClosedException(journalFile)
      // Exception when file has been deleted
      val result = new FileEventIterator[E](journalMeta, journalFile, tornEventId = tornEventId, flushedLength) {
        private val number = lentIterators.size + 1
        logger.debug(s"Opened $toString")

        override def close() = {
          synchronized {
            freeIterators -= this
            lentIterators -= this
          }
          super.close()
          logger.debug(s"Closed $toString")
        }

        override def toString = s"${super.toString} #$number)"
      }
      lentIterators += result
      result
    }

  def returnIterator(iterator: FileEventIterator[E]): Unit = {
    lazy val PositionAnd(position, eventId) = iterator.positionAndEventId
    logger.trace(s"returnIterator $iterator eventId=$eventId position=$position")
    synchronized {
      if (!iterator.isClosed) {
        freeIterators += iterator
      }
      lentIterators -= iterator
    }
  }

  override def toString = s"FileEventIteratorPool(${journalFile.getFileName})"

  def isClosed = closed()

  def isLent = lentIterators.nonEmpty

  def freeIteratorsCount = freeIterators.size

  def lentIteratorsCount = lentIterators.size
}

object FileEventIteratorPool {
  private val logger = Logger(getClass)
}
