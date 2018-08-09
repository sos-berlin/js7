package com.sos.jobscheduler.core.event.journal.watch

import com.google.common.annotations.VisibleForTesting
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.{CloseableIterator, DuplicateKeyException}
import com.sos.jobscheduler.common.event.RealEventWatch
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.listJournalFiles
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch._
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.typesafe.config.{Config, ConfigFactory}
import java.io.IOException
import java.nio.file.Files.delete
import java.nio.file.Path
import monix.eval.Task
import monix.execution.atomic.{AtomicAny, AtomicLong}
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.concurrent.{ExecutionContext, Promise}

/**
  * Watches a complete journal consisting of n `JournalFile`.
  * The last one (with highest after-EventId) is the currently written file while the others are historic.
  * @author Joacim Zschimmer
  */
final class JournalEventWatch[E <: Event](journalMeta: JournalMeta[E], config: Config)
  (implicit
    protected val executionContext: ExecutionContext,
    protected val timerService: TimerService)
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
      .map(o ⇒ HistoricJournalFile(o.afterEventId, o.file))
      .toKeyedMap(_.afterEventId)
  private val started = Promise[this.type]()
  @volatile
  private var currentEventReaderOption: Option[CurrentEventReader[E]] = None
  private val eventsAcceptedUntil = AtomicLong(EventId.BeforeFirst)

  def close() = {
    afterEventIdToHistoric.values foreach (_.close())
    currentEventReaderOption foreach (_.close())
  }

  override def whenStarted: Task[this.type] =
    Task.deferFuture(started.future)

  private def currentEventReader =
    currentEventReaderOption getOrElse (throw new IllegalStateException(s"$toString: Journal is not yet ready"))

  def onJournalingStarted(file: Path, flushedLengthAndEventId: PositionAnd[EventId]): Unit = {
    synchronized {
      val after = flushedLengthAndEventId.value
      if (after < lastEventId) throw new IllegalArgumentException(s"Invalid onJournalingStarted(after=$after), must be > $lastEventId")
      for (current ← currentEventReaderOption) {
        if (current.lastEventId != after)
          throw new DuplicateKeyException(s"onJournalingStarted($after) does not match lastEventId=${current.lastEventId}")
        for (o ← afterEventIdToHistoric.get(current.tornEventId)) o.close()  // In case last journal file had no events (and `after` remains)
        afterEventIdToHistoric += current.tornEventId → HistoricJournalFile(current.tornEventId, current.journalFile)
      }
      val reader = new CurrentEventReader[E](journalMeta, flushedLengthAndEventId, config)
      reader.start()
      currentEventReaderOption = Some(reader)
    }
    onEventsAdded(eventId = flushedLengthAndEventId.value)  // Notify about already written events
    started.trySuccess(this)
  }

  def tornEventId =
    synchronized {
      if (afterEventIdToHistoric.nonEmpty)
        afterEventIdToHistoric.keys.min
      else
        currentEventReader.tornEventId
    }

  @tailrec
  def onEventsAcceptedUntil(eventId: EventId): Unit = {
    val old = eventsAcceptedUntil()
    if (eventId < old)
      logger.warn(s"onEventsAcceptedUntil with already accepted EventId $eventId < $old ?")  // Not expected to happen. No exception here!
    else
    if (eventId > old) {
      if (!eventsAcceptedUntil.compareAndSet(old, eventId))
        onEventsAcceptedUntil(eventId)  // Try again when concurrently called
      else
        deleteObsoleteArchives()
    }
  }

  protected[journal] def deleteObsoleteArchives(): Unit = {
    val untilEventId = eventsAcceptedUntil()
    val keepAfter = currentEventReaderOption match {
      case Some(current) if current.tornEventId <= untilEventId ⇒
        current.tornEventId
      case _ ⇒
        historicAfter(untilEventId).fold(EventId.BeforeFirst)(_.afterEventId)  // Delete only journal files before the file containing untilEventId
    }
    for (historic ← afterEventIdToHistoric.values if historic.afterEventId < keepAfter) {
      try {
        logger.info(s"Deleting obsolete journal file '$historic'")
        historic.close()
        delete(historic.file)
        afterEventIdToHistoric -= historic.afterEventId
      } catch {
        case e: IOException ⇒ logger.error(s"Cannot delete obsolete journal file '$historic': ${e.toStringWithCauses}")
      }
    }
  }

  protected[journal] def onEventsAdded(flushedPositionAndEventId: PositionAnd[EventId], n: Int): Unit = {
    currentEventReader.onEventsAdded(flushedPositionAndEventId, n = n)
    onEventsAdded(eventId = flushedPositionAndEventId.value)
  }

  /**
    * @return `Task(None)` torn, `after` < `tornEventId`
    *         `Task(Some(Iterator.empty))` if no events are available for now
    */
  def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]] = {
    tryToEvict(until = after)
    currentEventReaderOption match {
      case Some(current) if current.tornEventId <= after ⇒
        current.eventsAfter(after)
      case _ ⇒
        historicEventsAfter(after)
    }
  }

  private def historicEventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]] =
    historicAfter(after) flatMap { historicJournalFile ⇒
      var last = after
      historicJournalFile.eventReader.eventsAfter(after) map { events ⇒
        events.map { stamped ⇒
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

  /** To reduce heap usage (for EventIdPositionIndex). **/
  private def tryToEvict(until: EventId): Unit =
    afterEventIdToHistoric.values
      .filter(o ⇒ o.afterEventId < until && o.isEvictable)
      .toVector.sortBy(_.lastUsedAt)
      .dropRight(keepOpenCount)
      .foreach(_.evictEventReader())

  protected def reverseEventsAfter(after: EventId) =
    CloseableIterator.empty  // Not implemented

  private def historicAfter(after: EventId): Option[HistoricJournalFile] =
    afterEventIdToHistoric.values.toVector.reverseIterator find (_.afterEventId <= after)

  @VisibleForTesting
  private[watch] def historicFileEventIds: Set[EventId] =
    afterEventIdToHistoric.keySet

  private def lastEventId =
    currentEventReaderOption match {
      case Some(o) ⇒ o.lastEventId
      case None if afterEventIdToHistoric.nonEmpty ⇒ afterEventIdToHistoric.keys.max
      case None ⇒ EventId.BeforeFirst
    }

  private final case class HistoricJournalFile(afterEventId: EventId, file: Path)
  {
    private val historicEventReader = AtomicAny[HistoricEventReader[E]](null)

    def close(): Unit =
      for (r ← Option(historicEventReader.get)) r.close()

    @tailrec
    def eventReader: HistoricEventReader[E] =
      historicEventReader.get match {
        case null ⇒
          val r = new HistoricEventReader[E](journalMeta, tornEventId = afterEventId, file, config)
          r.start()
          if (historicEventReader.compareAndSet(null, r))
            r
          else {
            r.close()
            eventReader
          }
        case r ⇒ r.asInstanceOf[HistoricEventReader[E]]
      }

    def evictEventReader(): Unit = {
      val reader = historicEventReader.get
      if (reader != null) {
        if (!reader.isInUse && historicEventReader.compareAndSet(reader, null)) {
          logger.debug(s"Evicted '${file.getFileName}' lastUsedAt=${reader.lastUsedAt}")
          reader.close()
        }
      }
    }

    def lastUsedAt: Timestamp =
      historicEventReader.get match {
        case null ⇒ Timestamp.Epoch
        case reader ⇒ reader.lastUsedAt
      }

    def isEvictable: Boolean = {
      val reader = historicEventReader.get
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
