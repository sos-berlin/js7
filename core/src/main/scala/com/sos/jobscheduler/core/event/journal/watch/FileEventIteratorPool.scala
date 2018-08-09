package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.FileEventIteratorPool._
import com.sos.jobscheduler.data.event.{Event, EventId}
import java.nio.file.Path
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
  @volatile
  private var closed = false

  def close(): Unit = {
    closed = true
    val (availables, lent): (Set[FileEventIterator[E]], Set[FileEventIterator[E]]) =
      synchronized {
        (freeIterators.toSet, lentIterators.toSet)
      }
    if (lent.nonEmpty) {
      logger.info(s"Close '$toString' while ${lent.size}× opened")  // May abort open web requests
    }
    (availables ++ lent) foreach (_.close())  // Force close iterators, for Windows to unlock the file - required for testing
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
    if (closed) throw new ClosedException
    tryBorrowIterator() getOrElse newIterator()
  }

  private def tryBorrowIterator(): Option[FileEventIterator[E]] =
    synchronized {
      freeIterators.nonEmpty ? {
        val result = freeIterators.remove(freeIterators.size - 1)  // LIFO
        lentIterators += result
        //logger.trace(s"borrowIterator ${journalFile.getFileName} position=${result.position} eventId=${result.eventId}")
        result
      }
    }

  private def newIterator(): FileEventIterator[E] =
    synchronized {
      // Exception when file has been deleted
      val result = new FileEventIterator[E](journalMeta, journalFile, tornEventId = tornEventId, flushedLength) {
        private val number = lentIterators.size + 1
        logger.trace(s"Opened $toString")
        override def close() = {
          logger.trace(s"Close  $toString")
          synchronized {
            freeIterators -= this
            lentIterators -= this
          }
          super.close()
        }
        override def toString = s"FileEventIterator(${journalFile.getFileName} #$number)"
      }
      lentIterators += result
      result
    }

  def returnIterator(iterator: FileEventIterator[E]): Unit = {
    //logger.trace(s"returnIterator ${journalFile.getFileName} position=${iterator.position} eventId=${iterator.eventId}")
    synchronized {
      if (!iterator.isClosed) {
        freeIterators += iterator
      }
      lentIterators -= iterator
    }
  }

  def isLent = lentIterators.nonEmpty

  def freeIteratorsCount = freeIterators.size

  def lentIteratorsCount = lentIterators.size
}

object FileEventIteratorPool {
  private val logger = Logger(getClass)
}
