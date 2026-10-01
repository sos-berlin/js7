package js7.journal.memory

import cats.effect.std.{AtomicCell, Mutex}
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.traverse.*
import fs2.concurrent.SignallingRef
import js7.base.catsutils.CatsEffectExtensions.{left, raceMerge, right}
import js7.base.catsutils.CatsExtensions.ifTrue
import js7.base.catsutils.Environment.environmentOr
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.system.startup.StartUp
import js7.base.time.ScalaTime.*
import js7.base.time.WallClock
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.cluster.ClusterState
import js7.data.event.{AnyKeyedEvent, Event, EventId, JournalId, JournalInfo, JournaledState, KeyedEvent, Stamped, TimeCtx}
import js7.journal.log.JournalLogger
import js7.journal.memory.MemoryJournal.*
import js7.journal.watch.RealEventWatch
import js7.journal.{EventIdGenerator, Journal, Persist, Persisted, StreamableJournal}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.{Deadline, FiniteDuration}

final class MemoryJournal[S <: JournaledState[S]] private(
  initial: S,
  size: Int,
  waitingFor: String,
  infoLogEvents: Set[String],
  eventIdGenerator: EventIdGenerator,
  clock: WallClock,
  semaphore: OurSemaphore,
  queueMutex: Mutex[IO],
  persistMutex: Mutex[IO],
  suppressStoringSignal: SignallingRef[IO, Boolean],
  onPersisted: AtomicCell[IO, Set[OnPersisted[S]]])
  (using protected val S: JournaledState.Companion[S])
extends
  Journal[S], StreamableJournal[S](chunkSize = size), Service.Trivial:

  val journalId: JournalId = JournalId.random()

  @volatile private var queue = EventQueue(EventId.BeforeFirst, EventId.BeforeFirst, Vector.empty)
  @volatile private var _aggregate = initial
  @volatile private var eventWatchStopped = false
  private var _eventCount = 0L

  private val journalLogger = new JournalLogger("memory", infoLogEvents, suppressTiming = true)

  def isHalted = false

  @TestOnly private[journal] def isEmpty = queue.events.isEmpty

  val whenNoFailoverByOtherNode: IO[Unit] = IO.unit

  val eventWatch: RealEventWatch =
    new RealEventWatch:
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

  def aggregate: IO[S] =
    IO(_aggregate)

  def unsafeAggregate(): S =
    _aggregate

  def unsafeUncommittedAggregate(): S =
    _aggregate

  override protected def persistSingle[E <: Event](persist: Persist[S, E])
  : IO[Checked[Persisted[S, E]]] =
    // TODO? Use the FileJournal's streaming queuing algorithm for bigger chunks and less
    //  EventWatch signals
    persistMutex.lock.surround:
      IO(_aggregate).flatMap: aggregate =>
        locally:
          for
            coll <- persist.eventCalc.calculate(aggregate, TimeCtx(clock.now(), StartUp.elapsed))
            stampedEvents = coll.timestampedKeyedEvents.map(eventIdGenerator.stamp)
            updatedAggr <- aggregate.applyKeyedEvents(coll.keyedEvents)
            persisted = Persisted(aggregate, stampedEvents, updatedAggr)
          yield
            IO.uncancelable: poll =>
              val n = stampedEvents.length
              poll:
                logEventsAfter(10.s, stampedEvents) // For diagnosis
              .background.surround:
                semaphore.acquireN(n, poll)
              .logWhenItTakesLonger(waitingFor)
              .as(true)
              .raceMerge:
                suppressStoringSignal.discrete.exists(identity).compile.drain.map: _ =>
                  logger.debug:
                    s"🪱 Not storing ${stampedEvents.length} events due to 'suppressStoring':"
                  stampedEvents.foreachWithBracket(): (stamped, br) =>
                    logger.debug:
                      s"🪱 Suppressed: $br${
                        stamped.value.toString.truncateWithEllipsis(300, firstLineOnly = true)}"
                  false
              .flatMap: acquired =>
                // acquired is false when suppressStoring is set, in which case we don't enqueue
                IO.whenA(acquired):
                  enqueue(stampedEvents, updatedAggr)
              .productR:
                persisted.ifNonEmpty:
                  onPersisted.get.flatMap:
                    _.toSeq.foldMapMI: onPersisted =>
                      poll:
                        onPersisted(persisted)
            .as(persisted)
        .sequence

  private def logEventsAfter(
    duration: FiniteDuration,
    stampedEvents: Seq[Stamped[AnyKeyedEvent]])
  : IO[Unit] =
    IO(logger.isTraceEnabled).ifTrue:
      def logEvents(label: String, events: Seq[Any]) =
        events.foreachWithBracket(): (s, br) =>
          logger.trace:
            s"🐌 $label $br${s.toString.truncateWithEllipsis(300, firstLineOnly = true)}"
      IO.sleep(duration).map: _ =>
        // Despite logWhenItTakesLonger below, we log the events for better diagnose
        logger.trace(s"🐌 Hanging in semaphore.acquireN(${stampedEvents.length}), queue=${
          queue.events.size} events, size=$size")
        logEvents("queue:  ", queue.events.map(_.value))
        logEvents("persist:", stampedEvents.map(_.value))

  private def enqueue[E <: Event](stampedEvents: Seq[Stamped[KeyedEvent[E]]], aggregate: S)
  : IO[Unit] =
    IO.whenA(stampedEvents.nonEmpty):
      IO(Deadline.now).flatMap: since =>
        queueMutex.lock.surround:
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
    queueMutex.lock.surround:
      IO.defer:
        val q = queue
        if untilEventId == q.tornEventId then
          IO.right(())
        else
          val (index, found) = queue.search(untilEventId)
          if !found then
            IO.left(Problem.pure(s"Unknown EventId: ${EventId.toString(untilEventId)}"))
          else
            val n = index + 1
            queue = q.copy(
              tornEventId = untilEventId,
              events = q.events.drop(n))
            semaphore.releaseN(n)
              .as(Checked.unit)

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
        Some:
          q.events.drop(index + found.toInt)
            .toList // release memory as iterator advances
            .iterator

  /** MemoryJournal pretends to persist, but it doesn't fill the queue.
    *
    * Call this when the Directory doesn't fetch and release the the events.
    * Then, `persist` no more enqueues events but pretends to work as normal. */
  def suppressStoring: IO[Unit] =
    IO(logger.debug("suppressStoring❗️")) *>
      suppressStoringSignal.set(true)

  def suppressLogging(suppress: Boolean): Unit =
    journalLogger.suppress(suppress)

  /** To simulate sudden death. */
  @TestOnly
  def stopEventWatch(): Unit =
    eventWatchStopped = true

  @TestOnly
  private[journal] def queueLength = queue.events.size

  /** Immediately after persisting, atomically call the `callback` if Persisted is not empty. */
  def registerOnPersistedCallback(callback: OnPersisted[S]): ResourceIO[Unit] =
    Resource.make(
      acquire = onPersisted.update: set =>
        if set.contains(callback) then throw IllegalArgumentException:
          s"registerOnPersistedCallback: Duplicate callback: $callback"
        set + callback)(
      release = _ => onPersisted.update(_ - callback))

  private sealed case class EventQueue(
    tornEventId: EventId,
    lastEventId: EventId,
    events: Vector[Stamped[KeyedEvent[Event]]]):

    def search(after: EventId): (Int, Boolean) =
      binarySearch(events, _.eventId)(after)


object MemoryJournal:

  type OnPersisted[S <: JournaledState[S]] = Persisted[S, Event] => IO[Unit]

  private val logger = Logger[this.type]

  def service[S <: JournaledState[S]](
    initial: S,
    size: Int,
    waitingFor: String = "releaseEvents",
    infoLogEvents: Set[String] = Set.empty,
    eventIdGenerator: EventIdGenerator = new EventIdGenerator)
    (using JournaledState.Companion[S])
  : ResourceIO[MemoryJournal[S]] =
    Resource.suspend:
      for
        clock <- environmentOr[WallClock](WallClock)
        semaphore <- OurSemaphore(size)
        queueMutex <- Mutex[IO]
        persistMutex <- Mutex[IO]
        suppressStoringSignal <- SignallingRef[IO, Boolean](false)
        onPersisted <- AtomicCell[IO].of(Set.empty[OnPersisted[S]])
      yield
        Service.resource:
          new MemoryJournal(initial, size, waitingFor, infoLogEvents, eventIdGenerator,
            clock, semaphore, queueMutex, persistMutex, suppressStoringSignal,
            onPersisted)
