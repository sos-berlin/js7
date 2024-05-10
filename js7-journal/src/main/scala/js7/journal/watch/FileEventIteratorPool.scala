package js7.journal.watch

import java.nio.file.Path
import js7.base.log.Logger
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalId}
import js7.journal.data.JournalLocation
import js7.journal.watch.FileEventIteratorPool.*
import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[watch] final class FileEventIteratorPool(
  journalLocation: JournalLocation,
  expectedJournalId: JournalId,
  journalFile: Path,
  fileEventId: EventId,
  committedLength: () => Long):

  private val freeIterators = mutable.ArrayBuffer.empty[FileEventIterator]
  private val lentIterators = mutable.ArrayBuffer.empty[FileEventIterator]
  private val closed = Atomic(false)

  def close(): Unit =
    synchronized:
      if !closed.getAndSet(true) then
        val (availables, lent): (Set[FileEventIterator], Set[FileEventIterator]) =
          synchronized:
            (freeIterators.toSet, lentIterators.toSet)
        if lent.nonEmpty then
          logger.info(s"Close '$toString' while ${lent.size}× opened")  // May abort open web requests
        (availables ++ lent) foreach (_.close())  // Force close iterators, for Windows to unlock the file - required for testing

  lazy val firstEventPosition: Long =
    val iterator = borrowIterator()
    try iterator.firstEventPosition
    catch { case NonFatal(t) =>
      iterator.close()
      throw t
    }
    finally returnIterator(iterator)   // JsonSeqReader is positioned after fileEventId

  def borrowIterator(): FileEventIterator =
    if closed.get() then throw new ClosedException(journalFile)
    tryBorrowIterator() getOrElse newIterator()

  private def tryBorrowIterator(): Option[FileEventIterator] =
    synchronized:
      freeIterators.nonEmpty ? {
        val iterator = freeIterators.remove(freeIterators.size - 1) // LIFO
        lentIterators += iterator
        logger.trace(s"borrowIterator $iterator eventId=${iterator.eventId} position=${iterator.position}")
        iterator
      }

  private def newIterator(): FileEventIterator =
    synchronized:
      if closed.get() then throw new ClosedException(journalFile)
      // Exception when file has been deleted
      val result = new FileEventIterator(
        journalLocation.S, journalFile,
        expectedJournalId,
        fileEventId = fileEventId,
        committedLength
      ):
        private val number = lentIterators.size + 1
        logger.debug(s"Opened $toString")

        override def close(): Unit =
          synchronized:
            freeIterators -= this
            lentIterators -= this
          super.close()
          logger.debug(s"Closed $toString")

        override def toString = s"${super.toString}#$number"
      lentIterators += result
      result

  def returnIterator(iterator: FileEventIterator): Unit =
    lazy val PositionAnd(position, eventId) = iterator.positionAndEventId
    logger.trace(s"returnIterator $iterator eventId=$eventId position=$position")
    synchronized:
      if !iterator.isClosed then
        freeIterators += iterator
      lentIterators -= iterator

  override def toString = s"FileEventIteratorPool(${journalFile.getFileName})"

  def isClosed = closed.get()

  def isLent = lentIterators.nonEmpty

  def freeIteratorsCount = freeIterators.size

  def lentIteratorsCount = lentIterators.size


object FileEventIteratorPool:
  private val logger = Logger[this.type]
