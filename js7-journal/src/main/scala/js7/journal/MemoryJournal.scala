package js7.journal

import cats.effect.std.Semaphore
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.traverse.*
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.catsutils.Environment.environmentOr
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.WallClock
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.base.utils.{AsyncLock, Atomic, CloseableIterator}
import js7.data.cluster.ClusterState
import js7.data.event.{Event, EventId, JournalId, JournalInfo, JournaledState, KeyedEvent, Stamped, TimeCtx}
import js7.journal.MemoryJournal.*
import js7.journal.log.JournalLogger
import js7.journal.watch.RealEventWatch
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline

final class MemoryJournal[S <: JournaledState[S]] private(
  initial: S,
  val size: Int,
  waitingFor: String = "releaseEvents",
  infoLogEvents: Set[String],
  eventIdGenerator: EventIdGenerator = new EventIdGenerator,
  clock: WallClock,
  semaphore: Semaphore[IO])
  (implicit protected val S: JournaledState.Companion[S])
extends Journal[S], Service.StoppableByRequest:

  val journalId: JournalId = JournalId.random()

  private val aggregateLock = AsyncLock.dontLog() // Slow with many (>100000) acquirers: ("MemoryJournal.aggregate")
  private val queueLock = AsyncLock.dontLog()
  @volatile private var queue = EventQueue(EventId.BeforeFirst, EventId.BeforeFirst, Vector.empty)
  @volatile private var _aggregate = initial
  @volatile private var eventWatchStopped = false
  private var _eventCount = 0L

  private val journalLogger = new JournalLogger("memory", infoLogEvents, suppressTiming = true)

  def isHalted = false

  @TestOnly private[journal] def isEmpty = queue.events.isEmpty

  val whenNoFailoverByOtherNode: IO[Unit] = IO.unit

  val eventWatch: RealEventWatch = new RealEventWatch:
    protected val isActiveNode = true

    protected def eventsAfter(after: EventId) =
      eventsAfter_(after).map(CloseableIterator.fromIterator)

    def journalInfo: JournalInfo =
      val q = queue
      JournalInfo(
        lastEventId = q.lastEventId,
        tornEventId = q.tornEventId,
        journalFiles = Nil)

    def tornEventId: EventId =
      queue.tornEventId

    override def toString = "MemoryJournal.EventWatch"

  def start =
    startService(untilStopRequested) // Nothing to do

  def aggregate: IO[S] =
    IO(_aggregate)

  def unsafeAggregate(): S =
    _aggregate

  def unsafeUncommittedAggregate(): S =
    _aggregate

  protected def persist_[E <: Event](persist: Persist[S, E]): IO[Checked[Persisted[S, E]]] =
    aggregateLock.lock:
      IO.defer:
        val aggregate = _aggregate
        locally:
          for
            coll <- persist.eventCalc.calculate(aggregate, TimeCtx(clock.now()))
            updated <- aggregate.applyKeyedEvents(coll.keyedEvents)
            stampedEvents = coll.timestampedKeyedEvents.map: o =>
              eventIdGenerator.stamp(o.keyedEvent, o.maybeMillisSinceEpoch)
          yield
            // Limit acq to size to allow more stampedEvents than size.
            // FIXME But then, the late releaseN releases to much, shifting the semaphore limit upwards.
            // In our reality, this should not happen because size >> stampedEvents.length.
            // size is >= 1000, and stampedEvents contains some OrderStdWritten or OrderProcessed.
            // It may happen with big stdout output.
            // We need a semaphore which allows bigger a acquisition, acquisition of the whole queue.
            val acq = stampedEvents.length min size
            semaphore.acquireN(acq)
              .logWhenItTakesLonger(waitingFor)
              .*>(
                enqueue(stampedEvents, updated))
              .as(Persisted(aggregate, stampedEvents, updated))
        .sequence

  private def enqueue[E <: Event](stampedEvents: Seq[Stamped[KeyedEvent[E]]], aggregate: S): IO[Unit] =
    IO.whenA(stampedEvents.nonEmpty):
      val since = Deadline.now
      queueLock.lock:
        IO:
          var q = queue
          val eventId = stampedEvents.last.eventId
          q = q.copy(
            events = q.events ++ stampedEvents,
            lastEventId = eventId)
          _aggregate = aggregate.withEventId(eventId)
          log(_eventCount + 1, stampedEvents, since)
          _eventCount += stampedEvents.length
          eventWatch.onEventsCommitted(eventId)
          queue = q

  private def log(
    eventNumber: Long, stampedEvents: Seq[Stamped[KeyedEvent[Event]]], since: Deadline)
  : Unit =
    journalLogger.logCommitted(
      //CorrelId.current,
      stampedEvents,
      eventNumber = eventNumber,
      since,
      clusterState = ClusterState.Empty.getClass.simpleScalaName)

  def releaseEvents(untilEventId: EventId): IO[Checked[Unit]] =
    queueLock.lock(IO.defer:
      val q = queue
      val (index, found) = queue.search(untilEventId)
      if !found then
        IO.left(Problem.pure(s"Unknown EventId: ${EventId.toString(untilEventId)}"))
      else
        val n = index + 1
        queue = q.copy(
          tornEventId = untilEventId,
          events = q.events.drop(n))

        semaphore.releaseN(n)
          .*>(semaphore.available.map(available => assertThat(available >= 0)))
          .pipeIf(isTest && false/*FIXME*/):
            _.<*(semaphore.count.flatTap(cnt => IO:
              if cnt > size then
                val msg = s"MemoryJournal: Semaphore is greater than queue size: $cnt > $size "
                logger.error(msg)
                throw new IllegalStateException(msg)))
          .as(Checked.unit))

  private def eventsAfter_(after: EventId): Option[Iterator[Stamped[KeyedEvent[Event]]]] =
    val q = queue
    if after < q.tornEventId then
      None
    else
      val (index, found) = q.search(after)
      if !found && after != q.tornEventId then
        None
      else if eventWatchStopped then
        Some(Iterator.empty)
      else
        Some(q.events.drop(index + found.toInt).iterator)

  def suppressLogging(suppress: Boolean): Unit =
    journalLogger.suppress(suppress)

  /** To simulate sudden death. */
  @TestOnly
  def stopEventWatch(): Unit =
    eventWatchStopped = true

  @TestOnly
  private[journal] def queueLength = queue.events.size

  @TestOnly
  private[journal] def semaphoreCount: IO[Long] =
    semaphore.count.flatTap: n =>
      IO(assertThat(n <= size))


  private sealed case class EventQueue(
    tornEventId: EventId,
    lastEventId: EventId,
    events: Vector[Stamped[KeyedEvent[Event]]]):

    def search(after: EventId): (Int, Boolean) =
      binarySearch(events, _.eventId)(after)


object MemoryJournal:

  private val logger = Logger[this.type]

  def resource[S <: JournaledState[S]](
    initial: S,
    size: Int,
    waitingFor: String = "releaseEvents",
    infoLogEvents: Set[String] = Set.empty,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator)
    (using JournaledState.Companion[S])
  : ResourceIO[MemoryJournal[S]] =
    for
      clock <- Resource.eval(environmentOr[WallClock](WallClock))
      semaphore <- Resource.eval(Semaphore[IO](size))
      memoryJournal <- Service.resource:
        new MemoryJournal(initial, size, waitingFor, infoLogEvents, eventIdGenerator,
          clock, semaphore)
    yield
      memoryJournal
