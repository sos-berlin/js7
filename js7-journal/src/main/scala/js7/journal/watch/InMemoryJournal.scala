package js7.journal.watch

import js7.base.problem.Checked
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{AsyncLock, CloseableIterator}
import js7.data.event.{Event, EventId, JournalInfo, JournaledState, KeyedEvent, Stamped}
import js7.journal.log.JournalLogger
import js7.journal.log.JournalLogger.SimpleLoggable
import js7.journal.state.StatePersistence
import js7.journal.{CommitOptions, EventIdGenerator}
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now

final class InMemoryJournal[S <: JournaledState[S]](
  initial: S,
  eventIdGenerator: EventIdGenerator)
  (implicit protected val S: JournaledState.Companion[S])
extends StatePersistence[S] with RealEventWatch
{
  // TODO Use AsyncLock instead of synchronized
  private val lock = AsyncLock("InMemoryJournal")
  @volatile private var _tornEventId = EventId.BeforeFirst
  @volatile private var _lastEventId = EventId.BeforeFirst
  @volatile private var _queue = Vector.empty[Stamped[KeyedEvent[Event]]]
  @volatile private var _state = initial
  @volatile private var eventWatchStopped = false
  private var _eventCount = 0L

  private val journalLogger = new JournalLogger(
    syncOrFlushChars = "memory",
    infoLogEvents = Set.empty,
    suppressTiming = true)

  val waitUntilStarted = Task.unit

  def eventWatch = this

  protected def isActiveNode = true

  def tornEventId = _tornEventId

  def journalInfo: JournalInfo =
    synchronized {
      JournalInfo(
        lastEventId = _lastEventId,
        tornEventId = _tornEventId,
        journalFiles = Nil)
    }

  def currentState: S =
    _state

  def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistKeyedEvents(keyedEvent :: Nil)
      .map(_.map { case (events, s) => events.head -> s })

  def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persist(_ => Right(keyedEvents))

  def persistKeyedEventLater[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[Unit]] =
    persistKeyedEvent(keyedEvent, options)
      .rightAs(())

  def persist[E <: Event](stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    lock.lock(Task {
      synchronized {
        for {
          keyedEvents <- stateToEvents(_state)
          updated <- _state.applyEvents(keyedEvents)
          stampedEvents = keyedEvents.map(eventIdGenerator.stamp(_))
        } yield {
          val qLen = _queue.length
          _queue ++= stampedEvents
          val n = _queue.length - qLen
          if (n > 0) {
            _eventCount += n
            val eventId = _queue.last.eventId
            _lastEventId = eventId
            _state = updated.withEventId(eventId)
            log(_eventCount, stampedEvents)
            onEventsCommitted(eventId)
          }
          stampedEvents -> updated
        }
      }
    })

  private def log(eventNumber: Long, stampedEvents: Seq[Stamped[KeyedEvent[Event]]]): Unit =
    journalLogger.logCommitted(Vector(new SimpleLoggable(
      eventNumber = eventNumber,
      stampedEvents,
      isTransaction = false,
      since = now,
      isLastOfFlushedOrSynced = true)).view)

  def releaseEvents(untilEventId: EventId): Task[Unit] =
    lock.lock(Task {
      synchronized {
        val (index, found) =
          binarySearch(0, _queue.length, i => _queue(i).eventId.compare(untilEventId))
        if (!found) throw new IllegalArgumentException(
          s"Unknown EventId: ${EventId.toString(untilEventId)}")
        else if (index > 0) {
          _tornEventId = _queue(index).eventId
          _queue = _queue.drop(index)
        }
      }
    })

  protected def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[Event]]]] =
    eventsAfter_(after)
      .map(iterator => CloseableIterator.fromIterator(iterator))

  private def eventsAfter_(after: EventId): Option[Iterator[Stamped[KeyedEvent[Event]]]] = {
    val (queue, torn) = synchronized((_queue, _tornEventId))
    if (after < torn)
      None
    else {
      val (index, found) = binarySearch(0, queue.length, i => queue(i).eventId.compare(after))
      if (!found && after != torn) {
        //Left(Problem.pure(s"Unknown ${EventId.toString(after)}"))
        None
      } else if (eventWatchStopped)
        Some(Iterator.empty)
      else
        Some(queue.drop(index + found.toInt).iterator)
    }
  }

  /** To simulate sudden death. */
  @TestOnly
  def stopEventWatch() =
    eventWatchStopped = true
}
