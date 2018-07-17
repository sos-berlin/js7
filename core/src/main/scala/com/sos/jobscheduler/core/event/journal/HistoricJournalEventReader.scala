package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, closeOnError}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd}
import com.sos.jobscheduler.core.event.journal.HistoricJournalEventReader._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.{Files, Path}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class HistoricJournalEventReader[E <: Event](journalMeta: JournalMeta[E], val afterEventId: EventId, journalFile: Path)
extends AutoCloseable
{
  import journalMeta.eventJsonCodec

  private val endPosition = Files.size(journalFile)
  private val eventIdToPositionIndex = new EventIdPositionIndex(size = 1000)

  logger.debug(s"journalFile=$journalFile afterEventId=$afterEventId")
  eventIdToPositionIndex.addAfter(afterEventId, firstEventPosition(journalFile))

  def close() = {}

  /**
    * @return `Task(None)` if `after` < `tornEventId`
    *         `Task(Some(Iterator.empty))` if no events are available for now
    */
  def eventsAfter(after: EventId): CloseableIterator[Stamped[KeyedEvent[E]]] = { // TODO Similar to CurrentJournalEventReader
    val position = eventIdToPositionIndex.positionAfter(after)
    if (position >= endPosition)  // Data behind endPosition is not flushed and probably incomplete
      CloseableIterator.empty
    else {
      val jsonFileReader = InputStreamJsonSeqReader.open(journalFile)  // Exception when file has been deleted since history.fileAfter
      closeOnError(jsonFileReader) {
        jsonFileReader.seek(position)
        new CloseableIterator[Stamped[KeyedEvent[E]]] {
          def close() = jsonFileReader.close()
          def hasNext = jsonFileReader.position != endPosition
          def next() = {
            val stamped = jsonFileReader.read().map(_.value)
              .getOrElse(sys.error(s"Unexpected end of journal file '$journalFile' at position ${jsonFileReader.position}, after=${EventId.toString(after)}"))
              .as[Stamped[KeyedEvent[E]]]
              .orThrow
            eventIdToPositionIndex.tryAddAfter(stamped.eventId, jsonFileReader.position)
            stamped
          }
        }.closeAtEnd
      }
      .dropWhile(_.eventId <= after)
    }
  }
}

object HistoricJournalEventReader {
  private val logger = Logger(getClass)

  private def firstEventPosition(journalFile: Path) =
    autoClosing(InputStreamJsonSeqReader.open(journalFile)) { jsonFileReader ⇒
      @tailrec def loop(): Long =
        jsonFileReader.read() match {
          case Some(PositionAnd(_, JournalWriter.EventsHeader)) ⇒ jsonFileReader.position
          case Some(_) ⇒ loop()
          case None ⇒ sys.error(s"Invalid journal file '$journalFile', EventHeader is missing")
        }
      loop()
    }
}
