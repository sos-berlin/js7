package js7.core.event.journal.watch

import java.nio.file.Path
import js7.base.utils.ScalaUtils.syntax._
import js7.common.event.PositionAnd
import js7.common.scalautil.Logger
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.watch.FileEventIteratorPool._
import js7.data.event.{EventId, JournalId}
import monix.execution.atomic.AtomicBoolean
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[watch] final class FileEventIteratorPool(
  journalMeta: JournalMeta,
  expectedJournalId: Option[JournalId],
  journalFile: Path,
  tornEventId: EventId,
  committedLength: () => Long)
{
  private val freeIterators = mutable.ArrayBuffer[FileEventIterator]()
  private val lentIterators = mutable.ArrayBuffer[FileEventIterator]()
  private val closed = AtomicBoolean(false)

  def close(): Unit =
    synchronized {
      if (!closed.getAndSet(true)) {
        val (availables, lent): (Set[FileEventIterator], Set[FileEventIterator]) =
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
    catch { case NonFatal(t) =>
      iterator.close()
      throw t
    }
    finally returnIterator(iterator)   // JsonSeqReader is positioned after tornEventId
  }

  def borrowIterator(): FileEventIterator = {
    if (closed()) throw new ClosedException(journalFile)
    tryBorrowIterator() getOrElse newIterator()
  }

  private def tryBorrowIterator(): Option[FileEventIterator] =
    synchronized {
      freeIterators.nonEmpty ? {
        val iterator = freeIterators.remove(freeIterators.size - 1)  // LIFO
        lentIterators += iterator
        logger.trace(s"borrowIterator $iterator eventId=${iterator.eventId} position=${iterator.position}")
        iterator
      }
    }

  private def newIterator(): FileEventIterator =
    synchronized {
      if (closed()) throw new ClosedException(journalFile)
      // Exception when file has been deleted
      val result = new FileEventIterator(journalMeta, journalFile, expectedJournalId, tornEventId = tornEventId, committedLength) {
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

        override def toString = s"${super.toString}#$number"
      }
      lentIterators += result
      result
    }

  def returnIterator(iterator: FileEventIterator): Unit = {
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
