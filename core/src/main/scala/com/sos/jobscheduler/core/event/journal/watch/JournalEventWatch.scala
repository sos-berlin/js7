package com.sos.jobscheduler.core.event.journal.watch

import com.google.common.annotations.VisibleForTesting
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.{CloseableIterator, DuplicateKeyException}
import com.sos.jobscheduler.common.event.RealEventWatch
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.listJournalFiles
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch._
import com.sos.jobscheduler.core.problems.ReverseKeepEventsProblem
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.problems.MasterRequiresUnknownEventIdProblem
import com.typesafe.config.{Config, ConfigFactory}
import java.io.IOException
import java.nio.file.Files.delete
import java.nio.file.Path
import monix.eval.Task
import monix.execution.atomic.{AtomicAny, AtomicLong}
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.concurrent.Promise

/**
  * Watches a complete journal consisting of n `JournalFile`.
  * The last one (with highest after-EventId) is the currently written file while the others are historic.
  * @author Joacim Zschimmer
  */
final class JournalEventWatch[E <: Event](val journalMeta: JournalMeta[E], expectedJournalId: Option[JournalId], config: Config)
extends AutoCloseable
with RealEventWatch[E]
with JournalingObserver
{
  private val keepOpenCount = config.getInt("jobscheduler.journal.watch.keep-open")
  // Read journal file names from directory while constructing
  @volatile
  private var afterEventIdToHistoric: SortedMap[EventId, HistoricJournalFile] =
    SortedMap.empty[EventId, HistoricJournalFile] ++
      listJournalFiles(journalMeta.fileBase)
      .map(o => new HistoricJournalFile(o.afterEventId, o.file))
      .toKeyedMap(_.afterEventId)
  private val started = Promise[this.type]()
  @volatile
  private var currentEventReaderOption: Option[CurrentEventReader[E]] = None
  private val keepEventsAfter = AtomicLong(EventId.BeforeFirst)

  def close() = {
    afterEventIdToHistoric.values foreach (_.close())
    currentEventReaderOption foreach (_.close())
  }

  override def whenStarted =
    started.future

  private def currentEventReader =
    currentEventReaderOption getOrElse (throw new IllegalStateException(s"$toString: Journal is not yet ready"))

  protected[journal] def onJournalingStarted(file: Path, flushedLengthAndEventId: PositionAnd[EventId]): Unit = {
    synchronized {
      val after = flushedLengthAndEventId.value
      if (after < lastEventId) throw new IllegalArgumentException(s"Invalid onJournalingStarted(after=$after), must be > $lastEventId")
      for (current <- currentEventReaderOption) {
        if (current.lastEventId != after)
          throw new DuplicateKeyException(s"onJournalingStarted($after) does not match lastEventId=${current.lastEventId}")
        for (o <- afterEventIdToHistoric.get(current.tornEventId)) {
          o.closeAfterUse()  // In case last journal file had no events (and `after` remains), we exchange it
        }
        afterEventIdToHistoric += current.tornEventId -> new HistoricJournalFile(
          afterEventId = current.tornEventId,
          current.journalFile,
          Some(current)/*Reuse built-up JournalIndex*/)
      }
      currentEventReaderOption = Some(new CurrentEventReader[E](journalMeta, expectedJournalId, flushedLengthAndEventId, config))
    }
    onEventsAdded(eventId = flushedLengthAndEventId.value)  // Notify about already written events
    started.trySuccess(this)
    evictUnusedEventReaders()
  }

  def checkEventId(eventId: EventId): Checked[Unit] =
    eventsAfter(eventId) match {
      case Some(iterator) =>
        iterator.close()
        Checked.unit
      case None =>
        Left(MasterRequiresUnknownEventIdProblem(requiredEventId = eventId))
    }

  def tornEventId =
    synchronized {
      if (afterEventIdToHistoric.nonEmpty)
        afterEventIdToHistoric.keys.min
      else
        currentEventReader.tornEventId
    }

  /** Files containing non-kept events may be deleted. */
  @tailrec
  def keepEvents(after: EventId): Checked[Completed] = {
    val old = keepEventsAfter.get
    if (after < old)
      Left(ReverseKeepEventsProblem(requestedAfter = after, currentAfter = old))
    else if (after == old)
      Checked.completed
    else if (!keepEventsAfter.compareAndSet(old, after))
      keepEvents(after)  // Try again when concurrently called
    else {
      deleteObsoleteJournalFiles()
      Checked.completed
    }
  }

  protected[journal] def deleteObsoleteJournalFiles(): Unit = {
    val after = keepEventsAfter.get
    val keepFileAfter = currentEventReaderOption match {
      case Some(current) if current.tornEventId <= after =>
        current.tornEventId
      case _ =>
        historicJournalFileAfter(after).fold(EventId.BeforeFirst)(_.afterEventId)  // Delete only journal files before the file containing `after`
    }
    for (historic <- afterEventIdToHistoric.values if historic.afterEventId < keepFileAfter) {
      logger.info(s"Deleting obsolete journal file '$historic'")
      historic.close()
      try {
        delete(historic.file)
        afterEventIdToHistoric -= historic.afterEventId
      } catch {
        case e: IOException => logger.warn(s"Cannot delete obsolete journal file '$historic': ${e.toStringWithCauses}")
      }
    }
  }

  protected[journal] def onEventsAdded(flushedPositionAndEventId: PositionAnd[EventId], n: Int): Unit = {
    currentEventReader.onEventsAdded(flushedPositionAndEventId, n = n)
    onEventsAdded(eventId = flushedPositionAndEventId.value)
  }

  def snapshotObjectsFor(after: EventId) =
    currentEventReaderOption match {
      case Some(current) if current.tornEventId <= after =>
        Some(current.tornEventId -> current.snapshotObjects)
      case _ =>
        historicJournalFileAfter(after)
          .map { historyJournalFile =>
            logger.debug(s"Reading snapshot from journal file '$historyJournalFile'")
            historyJournalFile.afterEventId -> historyJournalFile.eventReader.snapshotObjects
          }
    }

  /**
    * @return `Task(None)` torn, `after` < `tornEventId`
    *         `Task(Some(Iterator.empty))` if no events are available for now
    */
  def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]] = {
    val result = currentEventReaderOption match {
      case Some(current) if current.tornEventId <= after =>
        current.eventsAfter(after)
      case _ =>
        historicEventsAfter(after)
    }
    evictUnusedEventReaders()
    result
  }

  private def historicEventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]] =
    historicJournalFileAfter(after) flatMap { historicJournalFile =>
      var last = after
      historicJournalFile.eventReader.eventsAfter(after) map { events =>
        events.map { stamped =>
          last = stamped.eventId
          stamped
        } ++  // ++ is lazy, so last contains last read eventId
          (if (last == after)  // Nothing read
            CloseableIterator.empty
          else {  // Continue with next HistoricEventReader or CurrentEventReader
            assert(last > after, s"last=$last ≤ after=$after ?")
            eventsAfter(last) getOrElse CloseableIterator.empty  // Should never be torn here because last > after
          })
      }
    }

  /** Close unused HistoricEventReader. **/
  private def evictUnusedEventReaders(): Unit =
    afterEventIdToHistoric.values
      .filter(_.isEvictable)
      .toVector.sortBy(_.lastUsedAt)
      .dropRight(keepOpenCount)
      .foreach(_.evictEventReader())

  private def historicJournalFileAfter(after: EventId): Option[HistoricJournalFile] =
    afterEventIdToHistoric.values.toVector.reverseIterator find (_.afterEventId <= after)

  @VisibleForTesting
  private[watch] def historicFileEventIds: Set[EventId] =
    afterEventIdToHistoric.keySet

  private def lastEventId =
    currentEventReaderOption match {
      case Some(o) => o.lastEventId
      case None if afterEventIdToHistoric.nonEmpty => afterEventIdToHistoric.keys.max
      case None => EventId.BeforeFirst
    }

  private final class HistoricJournalFile(
    val afterEventId: EventId,
    val file: Path,
    initialEventReader: Option[EventReader[E]] = None)
  {
    private val _eventReader = AtomicAny[EventReader[E]](initialEventReader.orNull)

    def closeAfterUse(): Unit =
      for (r <- Option(_eventReader.get)) r.closeAfterUse()

    def close(): Unit =
      for (r <- Option(_eventReader.get)) r.close()

    @tailrec
    def eventReader: EventReader[E] =
      _eventReader.get match {
        case null =>
          val r = new HistoricEventReader[E](journalMeta, expectedJournalId, tornEventId = afterEventId, file, config)
          if (_eventReader.compareAndSet(null, r)) {
            logger.debug(s"Using HistoricEventReader(${file.getFileName})")
            r
          } else {
            r.close()
            eventReader
          }
        case r => r
      }

    def evictEventReader(): Unit = {
      val reader = _eventReader.get
      if (reader != null) {
        if (!reader.isInUse && _eventReader.compareAndSet(reader, null)) {  // Race condition, may be become in-use before compareAndSet
          logger.debug(s"Evict HistoricEventReader(${file.getFileName}' lastUsedAt=${Timestamp.ofEpochMilli(reader.lastUsedAt)})")
          reader.closeAfterUse()
        }
      }
    }

    def lastUsedAt: Long =
      _eventReader.get match {
        case null => 0L
        case reader => reader.lastUsedAt
      }

    def isEvictable: Boolean = {
      val reader = _eventReader.get
      reader != null && !reader.isInUse
    }

    override def toString = file.getFileName.toString
  }
}

object JournalEventWatch {
  private val logger = Logger(getClass)

  val TestConfig = ConfigFactory.parseString("""
     |jobscheduler.journal.watch.keep-open = 2
     |jobscheduler.journal.watch.index-size = 100
     |jobscheduler.journal.watch.index-factor = 10
    """.stripMargin)
}
