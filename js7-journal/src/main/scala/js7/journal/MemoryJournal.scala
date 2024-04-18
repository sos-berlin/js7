package js7.journal

import cats.effect.std.Semaphore
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.traverse.*
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.base.utils.{AsyncLock, Atomic, CloseableIterator, Tests}
import js7.data.event.{Event, EventId, JournalId, JournalInfo, JournaledState, KeyedEvent, Stamped}
import js7.journal.MemoryJournal.*
import js7.journal.log.JournalLogger
import js7.journal.log.JournalLogger.SimpleLoggable
import js7.journal.state.Journal
import js7.journal.watch.RealEventWatch
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline

final class MemoryJournal[S <: JournaledState[S]] private(
  initial: S,
  val size: Int,
  waitingFor: String = "releaseEvents",
  infoLogEvents: Set[String],
  eventIdGenerator: EventIdGenerator = new EventIdGenerator,
  semaphore: Semaphore[IO])
  (implicit protected val S: JournaledState.Companion[S])
extends Journal[S]:

  val journalId: JournalId = JournalId.random()

  private val stateLock = AsyncLock.dontLog() //Slow with many (>100000) acquirers: ("MemoryJournal.state")
  private val queueLock = AsyncLock.dontLog()
  @volatile private var queue = EventQueue(EventId.BeforeFirst, EventId.BeforeFirst, Vector.empty)
  @volatile private var _state = initial
  @volatile private var eventWatchStopped = false
  private val _eventCount = Atomic(0L)

  private val journalLogger = new JournalLogger("memory", infoLogEvents, suppressTiming = true)

  def isHalted = false

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

  def unsafeCurrentState(): S =
    _state

  def persistKeyedEvent[E <: Event](
    keyedEvent: KeyedEvent[E],
    options: CommitOptions = CommitOptions.default)
    (using enclosing: sourcecode.Enclosing)
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistKeyedEvents(keyedEvent :: Nil)
      .map(_.map { case (events, s) => events.head -> s })

  def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persist(_ => Right(keyedEvents))

  def persistKeyedEventsLater[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[Unit]] =
    persistKeyedEvents(keyedEvents, options)
      .rightAs(())

  def persistWithOptions[E <: Event](
    options: CommitOptions = CommitOptions.default)(
    stateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    stateLock.lock:
      IO.defer:
        val state = _state
        (for
          keyedEvents <- stateToEvents(state)
          updated <- state.applyEvents(keyedEvents)
          stampedEvents = keyedEvents.map(eventIdGenerator.stamp(_))
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
            .as(stampedEvents -> updated)
        ).sequence

  private def enqueue[E <: Event](stampedEvents: Seq[Stamped[KeyedEvent[E]]], state: S): IO[Unit] =
    IO.whenA(stampedEvents.nonEmpty):
      val since = Deadline.now
      queueLock.lock:
        IO:
          var q = queue
          val eventId = stampedEvents.last.eventId
          q = q.copy(
            events = q.events ++ stampedEvents,
            lastEventId = eventId)
          _state = state.withEventId(eventId)
          val n = _eventCount += stampedEvents.length
          log(n, stampedEvents, since)
          eventWatch.onEventsCommitted(eventId)
          queue = q

  private def log(
    eventNumber: Long, stampedEvents: Seq[Stamped[KeyedEvent[Event]]], since: Deadline)
  : Unit =
    journalLogger.logCommitted(Vector(new SimpleLoggable(
      CorrelId.current,
      eventNumber = eventNumber,
      stampedEvents,
      isTransaction = false,
      since = since,
      isLastOfFlushedOrSynced = true)).view)

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
        //Left(UnknownEventIdProblem(after, q.tornEventId, q.lastAddedEventId))
        None
      else if eventWatchStopped then
        Some(Iterator.empty)
      else
        Some(q.events.drop(index + found.toInt).iterator)

  def suppressLogging(supress: Boolean): Unit =
    journalLogger.suppress(supress)

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
    (implicit S: JournaledState.Companion[S])
  : ResourceIO[MemoryJournal[S]] =
    Resource.eval:
      start(initial, size, waitingFor, infoLogEvents, eventIdGenerator)

  def start[S <: JournaledState[S]](
    initial: S,
    size: Int,
    waitingFor: String = "releaseEvents",
    infoLogEvents: Set[String] = Set.empty,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator)
    (implicit S: JournaledState.Companion[S])
  : IO[MemoryJournal[S]] =
    Semaphore[IO](size).flatMap: semaphore =>
      IO:
        new MemoryJournal[S](initial, size, waitingFor, infoLogEvents, eventIdGenerator, semaphore)
