package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.{InputStreamJsonSeqReader, PositionAnd, SeekableInputStream}
import com.sos.jobscheduler.core.event.journal.data.JournalHeaders.EventsHeader
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.FileEventIterator._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import java.nio.file.Path
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
private[watch] class FileEventIterator[E <: Event](journalMeta: JournalMeta[E], journalFile: Path, tornEventId: EventId, flushedLength: () ⇒ Long)
extends CloseableIterator[Stamped[KeyedEvent[E]]]
{
  // Similar to JournalRecovererReader, but reads only events.
  // TODO Mit JournalRecovererReader zusammenlegen ?

  import journalMeta.eventJsonCodec

  private val logger = Logger.withPrefix[FileEventIterator[E]](journalFile.getFileName.toString)
  val jsonReader = new InputStreamJsonSeqReader(SeekableInputStream.openFile(journalFile))

  private var closed = false
  private var _eventId = UndefinedEventId
  private var skipTo: EventId = _
  private var skip = false
  private var skipped = 0

  def close(): Unit = {
    if (!closed) {
      closed = true
      jsonReader.close()
    }
  }

  lazy val firstEventPosition: Long = {
    if (jsonReader.position != 0) throw new IllegalStateException("FileEventIterator.firstEventPosition called after borrowIterator")
    Iterator.continually(jsonReader.read())
      .collectFirst {
        case Some(PositionAnd(_, EventsHeader)) ⇒ jsonReader.position
        case None ⇒ sys.error(s"Invalid journal file '$journalFile', EventHeader is missing")
      }
      .get
  }

  final def seek(position: Long): Unit = {
    _eventId = UndefinedEventId
    jsonReader.seek(position)
  }

  final def skipToEventAfter(after: EventId): Unit = {
    // Don't implement with dropWhile because its calls an extra next() for buffering,
    // requiring an extra seek() to position back for continuously event fetches.
    skipTo = after
    skip = true
  }

  final def hasNext = jsonReader.position != flushedLength()

  @tailrec
  final def next(): Stamped[KeyedEvent[E]] = {
    if (!hasNext) throw new NoSuchElementException
    val beforePosition = position
    val stamped = jsonReader.read().map(_.value)
      .getOrElse(sys.error(s"Unexpected end of journal files '$journalFile' at position ${jsonReader.position}, tornEventId=${EventId.toString(tornEventId)}"))
      .as[Stamped[KeyedEvent[E]]]
      .orThrow
    if (stamped.eventId <= _eventId) sys.error(s"Journal file '$journalFile' contains events in reverse order " +
      s" at position $beforePosition, ${EventId.toString(stamped.eventId)} ≤ ${EventId.toString(_eventId)}")
    _eventId = stamped.eventId
    if (skip && stamped.eventId <= skipTo) {
      skipped += 1
      next()
    } else {
      if (skipped > 0) logger.trace(s"$skipped events skipped after=$eventId")
      skipped = 0
      skip = false
      stamped
    }
  }

  final def eventId = (_eventId != UndefinedEventId) ? _eventId
  final def position = jsonReader.position
  final def isClosed = closed

  override def toString =
    s"FileEventIterator(${journalFile.getFileName} tornEventId=${EventId.toString(tornEventId)})"
}

object FileEventIterator {
  private val UndefinedEventId = EventId.BeforeFirst - 1
}
