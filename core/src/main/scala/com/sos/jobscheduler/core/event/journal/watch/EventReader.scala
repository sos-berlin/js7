package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.InputStreamJsonSeqReader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.EventReader._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.typesafe.config.Config
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[watch] trait EventReader[E <: Event]
extends AutoCloseable
{
  /** `endPosition` does not grow. */
  protected def isHistoric: Boolean
  protected val journalMeta: JournalMeta[E]
  protected def journalFile: Path
  protected def tornEventId: EventId
  protected def tornPosition: Long
  /** Must be constant if `isHistoric`. */
  protected def endPosition: Long
  protected def config: Config

  protected lazy val eventIdToPositionIndex = new EventIdPositionIndex(size = config.getInt("jobscheduler.journal.watch.index-size"))
  private lazy val readerPool = new JsonSeqReaderPool(journalFile)

  import journalMeta.eventJsonCodec

  eventIdToPositionIndex.addAfter(tornEventId, tornPosition)

  def close() = readerPool.close()

  protected final def untornEventsAfter(after: EventId): CloseableIterator[Stamped[KeyedEvent[E]]] = {
    val position = eventIdToPositionIndex.positionAfter(after)
    if (position >= endPosition)  // Data behind endPosition is not flushed and probably incomplete
      CloseableIterator.empty
    else {
      val jsonFileReader = borrowReader()
      closeOnError(jsonFileReader) {
        logger.trace(s"seek $position")
        jsonFileReader.seek(position)
        new EventCloseableIterator(jsonFileReader, after)
          .skipToEventAfter(after)
          .closeAtEnd
      }
    }
  }

  protected final def borrowReader() =
    readerPool.borrowReader()

  protected final def returnReader(reader: InputStreamJsonSeqReader) =
    readerPool.returnReader(reader)

  final def lastUsedAt: Timestamp =
    readerPool.lastUsedAt

  final def isInUse = readerPool.isLent

  private class EventCloseableIterator(jsonFileReader: InputStreamJsonSeqReader, after: EventId)
  extends CloseableIterator[Stamped[KeyedEvent[E]]]
  {
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
      if (isHistoric) {
        eventIdToPositionIndex.tryAddAfter(stamped.eventId, jsonFileReader.position)
        if (jsonFileReader.position == endPosition) {
          eventIdToPositionIndex.freeze(toFactor = config.getInt("jobscheduler.journal.watch.index-factor"))
        }
      }
      stamped
    }

    def skipToEventAfter(after: EventId): CloseableIterator[Stamped[KeyedEvent[E]]] = {
      var skipped = 0
      dropWhile { stamped ⇒
        val drop = stamped.eventId <= after
        if (drop) skipped += 1 else if (skipped > 0) { logger.trace(s"'${journalFile.getFileName}' $skipped events skipped"); skipped = 0 }
        drop
      }
    }

    override def toString = s"CloseableIterator(after = ${EventId.toString(after)}, ${readerPool.size}× open)"
  }
}

private object EventReader {
  private val logger = Logger(getClass)
}
