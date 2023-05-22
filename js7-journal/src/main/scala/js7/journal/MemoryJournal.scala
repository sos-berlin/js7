package js7.journal

import cats.effect.Resource
import cats.syntax.traverse.*
import js7.base.log.CorrelId
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, CloseableIterator}
import js7.data.event.{Event, EventId, JournalId, JournalInfo, JournaledState, KeyedEvent, Stamped}
import js7.journal.log.JournalLogger
import js7.journal.log.JournalLogger.SimpleLoggable
import js7.journal.state.Journal
import js7.journal.watch.RealEventWatch
import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution.atomic.Atomic
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now

final class MemoryJournal[S <: JournaledState[S]] private[journal](
  initial: S,
  size: Int,
  waitingFor: String = "releaseEvents",
  infoLogEvents: Set[String],
  eventIdGenerator: EventIdGenerator = new EventIdGenerator)
  (implicit protected val S: JournaledState.Companion[S])
extends Journal[S]
{
  val journalId = JournalId.random()

  private val stateLock = AsyncLock("MemoryJournal.state")
  private val queueLock = AsyncLock("MemoryJournal.queue")
  // TODO Pause journal if queue is events are not released for a long time, despite length of queue?
  private val semaphore = Semaphore[Task](size).memoize
  private val semaMininum = size max 1
  @volatile private var queue = EventQueue(EventId.BeforeFirst, EventId.BeforeFirst, Vector.empty)
  @volatile private var _state = initial
  @volatile private var eventWatchStopped = false
  private val _eventCount = Atomic(0L)

  private val journalLogger =
    new JournalLogger("memory", infoLogEvents, suppressTiming = true)

  val whenNoFailoverByOtherNode = Task.unit

  val eventWatch: RealEventWatch = new RealEventWatch {
    protected def eventsAfter(after: EventId) =
      eventsAfter_(after)
        .map(iterator => CloseableIterator.fromIterator(iterator))

    protected val isActiveNode = true

    def journalInfo: JournalInfo = {
      val q = queue
      JournalInfo(
        lastEventId = q.lastEventId,
        tornEventId = q.tornEventId,
        journalFiles = Nil)
    }

    def tornEventId: EventId =
      queue.tornEventId

    override def toString = "MemoryJournal.EventWatch"
  }

  def unsafeCurrentState(): S =
    _state

  def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
    (implicit enclosing: sourcecode.Enclosing)
  : Task[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistKeyedEvents(keyedEvent :: Nil)
      .map(_.map { case (events, s) => events.head -> s })

  def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persist(_ => Right(keyedEvents))

  def persistKeyedEventsLater[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : Task[Checked[Unit]] =
    persistKeyedEvents(keyedEvents, options)
      .rightAs(())

  def persistWithOptions[E <: Event](
    options: CommitOptions = CommitOptions.default,
    stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : Task[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    stateLock.lock(Task.defer {
      (for {
        keyedEvents <- stateToEvents(_state)
        updated <- _state.applyEvents(keyedEvents)
        stampedEvents = keyedEvents.map(eventIdGenerator.stamp(_))
      } yield
        semaphore.flatMap(_
          .acquireN(stampedEvents.length min semaMininum)
          .logWhenItTakesLonger(waitingFor)
          .*>(queueLock
            .lock(Task {
              var q = queue
              val qLen = q.events.length
              q = q.copy(events = q.events ++ stampedEvents)
              val n = q.events.length - qLen
              if (n > 0) {
                _eventCount += n
                val eventId = q.events.last.eventId
                q = q.copy(lastEventId = eventId)
                _state = updated.withEventId(eventId)
                log(_eventCount.get(), stampedEvents)
                eventWatch.onEventsCommitted(eventId)
              }
              queue = q
              stampedEvents -> updated
          })))
      ).sequence
    })

  private def log(eventNumber: Long, stampedEvents: Seq[Stamped[KeyedEvent[Event]]]): Unit =
    journalLogger.logCommitted(Vector(new SimpleLoggable(
      CorrelId.current,
      eventNumber = eventNumber,
      stampedEvents,
      isTransaction = false,
      since = now,
      isLastOfFlushedOrSynced = true)).view)

  def releaseEvents(untilEventId: EventId): Task[Checked[Unit]] =
    queueLock.lock(Task.defer {
      val q = queue
      val (index, found) =
        binarySearch(0, q.events.length, i => q.events(i).eventId.compare(untilEventId))
      if (!found)
        Task.pure(Left(Problem.pure(s"Unknown EventId: ${EventId.toString(untilEventId)}")))
      else {
        val n = index + 1
        queue = q.copy(
          tornEventId = untilEventId,
          events = q.events.drop(n))
        semaphore
          .flatMap(_.releaseN(n))
          .*>(semaphore.flatMap(_.available).map(available => assertThat(available >= 0)))
          .as(Checked.unit)
      }
    })

  private def eventsAfter_(after: EventId): Option[Iterator[Stamped[KeyedEvent[Event]]]] = {
    val q = queue
    if (after < q.tornEventId)
      None
    else {
      val (index, found) = binarySearch(0, q.events.length, i => q.events(i).eventId.compare(after))
      if (!found && after != q.tornEventId) {
        //Left(Problem.pure(s"Unknown ${EventId.toString(after)}"))
        None
      } else if (eventWatchStopped)
        Some(Iterator.empty)
      else
        Some(q.events.drop(index + found.toInt).iterator)
    }
  }

  def suppressLogging(supress: Boolean): Unit =
    journalLogger.suppress(supress)

  /** To simulate sudden death. */
  @TestOnly
  def stopEventWatch() =
    eventWatchStopped = true

  @TestOnly
  def queueLength = queue.events.size

  private sealed case class EventQueue(
    tornEventId: EventId,
    lastEventId: EventId,
    events: Vector[Stamped[KeyedEvent[Event]]])
}

object MemoryJournal {
  def resource[S <: JournaledState[S]](
    initial: S,
    size: Int,
    waitingFor: String = "releaseEvents",
    infoLogEvents: Set[String] = Set.empty,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator)
    (implicit S: JournaledState.Companion[S])
  : Resource[Task, MemoryJournal[S]] =
    Resource.eval(Task(
      new MemoryJournal[S](initial, size, waitingFor, infoLogEvents, eventIdGenerator)))
}
