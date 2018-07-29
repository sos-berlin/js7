package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.{Logger, ScalaConcurrentHashSet}
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.GenericJournalEventReader._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Path
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
private[watch] trait GenericJournalEventReader[E <: Event]
extends AutoCloseable
{
  protected val journalMeta: JournalMeta[E]
  protected def journalFile: Path
  protected def tornEventId: EventId
  protected def tornPosition: Long
  protected def endPosition: Long

  import journalMeta.eventJsonCodec

  protected val eventIdToPositionIndex = new EventIdPositionIndex(size = 10000)
  private lazy val cache = new Cache(journalFile)

  eventIdToPositionIndex.addAfter(tornEventId, tornPosition)

  def close() = cache.close()

  protected def untornEventsAfter(after: EventId): CloseableIterator[Stamped[KeyedEvent[E]]] = {
    val position = eventIdToPositionIndex.positionAfter(after)
    if (position >= endPosition)  // Data behind endPosition is not flushed and probably incomplete
      CloseableIterator.empty
    else {
      val iterator = newCloseableIterator(position = position, after = after).closeAtEnd
      skip(iterator, after = after)
    }
  }

  private def skip(iterator: CloseableIterator[Stamped[KeyedEvent[E]]], after: EventId) = {
    var skipped = 0
    iterator.dropWhile { stamped ⇒
      val drop = stamped.eventId <= after
      if (drop) skipped += 1 else if (skipped > 0) { logger.trace(s"'${journalFile.getFileName}' $skipped events skipped"); skipped = 0 }
      drop
    }
  }

  private def newCloseableIterator(position: Long, after: EventId) = {
    val jsonFileReader = borrowReader()
    closeOnError(jsonFileReader) {
      jsonFileReader.seek(position)
      new EventCloseableIterator(jsonFileReader, after)
    }
  }

  protected final def borrowReader() = cache.borrowReader()

  protected final def returnReader(reader: InputStreamJsonSeqReader) = cache.returnReader(reader)

  private class EventCloseableIterator(jsonFileReader: InputStreamJsonSeqReader, after: EventId) extends CloseableIterator[Stamped[KeyedEvent[E]]] {
    var closed = false

    def close() =
      if (!closed) {
        returnReader(jsonFileReader)
        closed = true
      }

    def hasNext = jsonFileReader.position != endPosition

    def next() = {
      val stamped = jsonFileReader.read().map(_.value)
        .getOrElse(sys.error(s"Unexpected end of journal files '$journalFile' at position ${jsonFileReader.position}, after=${EventId.toString(after)}"))
        .as[Stamped[KeyedEvent[E]]]
        .orThrow
      eventIdToPositionIndex.tryAddAfter(stamped.eventId, jsonFileReader.position)  // For HistoricJournalEventReader
      stamped
    }

    override def toString = s"CloseableIterator(after = ${EventId.toString(after)}, ${cache.size}× open)"
  }
}

private object GenericJournalEventReader {
  private val logger = Logger(getClass)

  private class Cache(file: Path) {
    private val availableReaders = new ConcurrentLinkedQueue[InputStreamJsonSeqReader]
    private val lentReaders = new ScalaConcurrentHashSet[InputStreamJsonSeqReader]
    @volatile
    private var closed = false

    def close(): Unit = {
      closed = true
      val (availables, lent): (Set[InputStreamJsonSeqReader], Set[InputStreamJsonSeqReader]) =
        synchronized {
          (availableReaders.asScala.toSet, lentReaders.toSet)
        }
      if (lent.nonEmpty) {
        logger.debug(s"Closing '$toString' while ${lent.size}× opened")
      }
      (availables ++ lent) foreach (_.close())  // Force close readers, for Windows to unlock the file - required for testing
    }

    def borrowReader(): InputStreamJsonSeqReader = {
      if (closed) throw new ClosedException(file)

      var result: InputStreamJsonSeqReader = null
      synchronized {
        result = availableReaders.poll()
        if (result != null) {
          lentReaders += result
        }
      }
      if (result == null) {
        result = InputStreamJsonSeqReader.open(file,  // Exception when file has been deleted
          onClose = { me ⇒
            availableReaders.remove(me)
            lentReaders -= me
          })
        // When close is called here the reader will not be closed. Good enough for JobScheduler use case.
        lentReaders += result
      }
      result
    }

    def returnReader(reader: InputStreamJsonSeqReader): Unit =
      availableReaders.synchronized {
        availableReaders.add(reader)
        lentReaders -= reader
      }

    def size = availableReaders.size + lentReaders.size
  }
}
