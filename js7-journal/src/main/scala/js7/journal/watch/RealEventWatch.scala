package js7.journal.watch

import cats.syntax.option._
import java.util.concurrent.TimeoutException
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.closeableIteratorToObservable
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.problem.{Checked, Problem}
import js7.base.stream.IncreasingNumberSync
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import js7.data.problems.UnknownEventIdProblem
import js7.journal.watch.RealEventWatch._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
trait RealEventWatch extends EventWatch
{
  protected def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[Event]]]]

  protected def isActiveNode: Boolean

  // Lazy, initialize only after whenStarted has been called!
  private lazy val committedEventIdSync =
    new IncreasingNumberSync(initial = tornEventId, o => "EventId " + EventId.toString(o))

  protected final def onEventsCommitted(eventId: EventId): Unit =
    committedEventIdSync.onAdded(eventId)

  final def observe[E <: Event](
    request: EventRequest[E],
    predicate: KeyedEvent[E] => Boolean,
    onlyAcks: Boolean)
  : Observable[Stamped[KeyedEvent[E]]] =
  {
    val originalTimeout = request.timeout
    var deadline = none[MonixDeadline]

    def next(lazyRequest: () => EventRequest[E])
    : Task[(Option[Observable[Stamped[KeyedEvent[E]]]], () => EventRequest[E])] =
      Task.deferAction { implicit s =>
        val request = lazyRequest()  // Access now in previous iteration computed values lastEventId and limit (see below)
        // Timeout is renewed after every fetched event
        deadline = request.timeout.map(t => now + (t min EventRequest.LongTimeout))
        if (request.limit <= 0)
          NoMoreObservable
        else
          when[E](request, predicate) map {
            case TearableEventSeq.Torn(tornAfter) =>
              throw new TornException(after = request.after, tornEventId = tornAfter)

            case EventSeq.Empty(lastEventId) =>
              val remaining = deadline.map(_.timeLeft)
              (remaining.forall(_.isPositive) ? Observable.empty,
                () => request.copy[E](after = lastEventId, timeout = deadline.map(_.timeLeftOrZero)))

            case EventSeq.NonEmpty(events) =>
              if (events.isEmpty) throw new IllegalStateException("EventSeq.NonEmpty(EMPTY)")  // Do not loop
              val iterator = if (onlyAcks) lastOfIterator(events) else events
              var lastEventId = request.after
              var limit = request.limit
              val observable = closeableIteratorToObservable(iterator)
                .map { o =>
                  lastEventId = o.eventId
                  limit -= 1
                  o
                }
              // Closed-over lastEventId and limit are updated as observable is consumed,
              // therefore defer access to final values (see above)
              (Some(observable),
                () => request.copy[E](after = lastEventId, limit = limit, timeout = originalTimeout))
          }
    }

    Observable.fromAsyncStateAction(next)(() => request)
      .takeWhile(_.nonEmpty)  // Take until limit reached (NoMoreObservable) or timeout elapsed
      .map(_.get).flatten
  }

  final def observeEventIds(maybeTimeout: Option[FiniteDuration]): Observable[EventId] =
  {
    val originalTimeout = maybeTimeout
    var deadline = none[MonixDeadline]

    def next(lazyAfter: () => (EventId, Option[FiniteDuration]))
    : Task[(Option[Observable[EventId]], () => (EventId, Option[FiniteDuration]))] =
      Task.deferAction { implicit s =>
        // Access now the in previous iteration computed lastEventId and limit values (see below)
        val (after, maybeTimeout) = lazyAfter()
        // Timeout is renewed after every fetched event
        deadline = maybeTimeout.map(t => now + (t min EventRequest.LongTimeout))
        committedEventIdSync.whenAvailable(after, deadline)
          .map {
            case false =>
              val remaining = deadline.map(_.timeLeft)
              logger.debug("committedEventIdSync.whenAvailable returned false, " +
                remaining.fold("")(o => ", remaining=" + o.pretty))
              (remaining.forall(_.isPositive) ? Observable.empty,
                () => after -> deadline.map(_.timeLeftOrZero))

            case true =>
              val lastEventId = lastAddedEventId
              (Some(Observable.pure(lastEventId)),
                () => lastEventId -> originalTimeout)
          }
          .flatMap { x =>
            if (isActiveNode)
              Task.raiseError(Problem
                .pure("This active cluster node does not provide event acknowledgements" +
                  " (two active cluster nodes?)")
                .throwable)
            else
              Task.pure(x)
          }
      }

    val lastEventId = lastAddedEventId
    lastEventId +:
      Observable.fromAsyncStateAction(next)(() => lastEventId -> maybeTimeout)
        .takeWhile(_.nonEmpty)  // Take until timeout elapsed
        .map(_.get).flatten
  }

  final def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]] =
      whenAny[E](request, request.eventClasses, predicate)

  final def whenAny[E <: Event](
    request: EventRequest[E],
    eventClasses: Set[Class[_ <: E]],
    predicate: KeyedEvent[E] => Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]] =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if eventClasses.exists(_ isAssignableFrom e.event.getClass)
          && predicate(e.asInstanceOf[KeyedEvent[E]]) =>
          e.asInstanceOf[KeyedEvent[E]]
      })

  final def whenKeyedEvent[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean)
  : Task[E] =
    whenKey[E](request.copy[E](limit = 1), key, predicate) map {
      case eventSeq: EventSeq.NonEmpty[CloseableIterator, E] =>
        try eventSeq.stamped.next().value
        finally eventSeq.close()
      case _: EventSeq.Empty => throw new TimeoutException(s"Timed out: $request")
      case _: TearableEventSeq.Torn => throw new IllegalStateException("EventSeq is torn")
    }

  final def whenKey[E <: Event](request: EventRequest[E], key: E#Key, predicate: E => Boolean)
  : Task[TearableEventSeq[CloseableIterator, E]] =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if request.matchesClass(e.event.getClass)
          && e.key == key && predicate(e.event.asInstanceOf[E]) =>
          e.event.asInstanceOf[E]
      })

  private def whenAnyKeyedEvents[E <: Event, A](
    request: EventRequest[E],
    collect: PartialFunction[AnyKeyedEvent, A])
  : Task[TearableEventSeq[CloseableIterator, A]] =
    Task.deferAction { implicit s =>
      // Protected agains Timestamp overflow
      val deadline = request.timeout.map(t => now + t.min(EventRequest.LongTimeout))
      whenAnyKeyedEvents2(request.after, deadline, request.delay, collect, request.limit,
        maybeTornOlder = request.tornOlder)
    }

  private def whenAnyKeyedEvents2[A](
    after: EventId,
    deadline: Option[MonixDeadline],
    delay: FiniteDuration,
    collect: PartialFunction[AnyKeyedEvent, A],
    limit: Int,
    maybeTornOlder: Option[FiniteDuration])
  : Task[TearableEventSeq[CloseableIterator, A]] = {
    val overheatingDurations = Iterator(10, 30, 70, 300, 600)
      .concat(Iterator.continually(1000))
      .map(_.ms)

    def untilNonEmpty(after: EventId): Task[TearableEventSeq[CloseableIterator, A]] =
      started
        .flatMap(_ => committedEventIdSync.whenAvailable(after, deadline, delay))
        .flatMap(eventArrived =>
          collectEventsSince(after, collect, limit) match {
            case eventSeq @ EventSeq.NonEmpty(iterator) =>
              maybeTornOlder match {
                case None => Task.pure(eventSeq)
                case Some(tornOlder) =>
                  // If the first event is not fresh, we have a read congestion.
                  // We serve a (simulated) Torn, and the client can fetch the current state
                  // and read fresh events, skipping the congestion.
                  Task.defer {
                    val head = iterator.next()
                    // Don't compare head.timestamp, timestamp may be much older)
                    if (EventId.toTimestamp(head.eventId) + tornOlder < Timestamp.now) {
                      iterator.close()
                      // Simulate a torn EventSeq
                      Task.pure(TearableEventSeq.Torn(committedEventIdSync.last))
                    } else
                      Task.pure(EventSeq.NonEmpty(head +: iterator))
                  }
              }

            case empty @ EventSeq.Empty(lastEventId) =>
              Task.defer {
                if (deadline.forall(_.hasTimeLeft)) {
                  val preventOverheating =
                    if (lastEventId < committedEventIdSync.last) {
                      // May happen due to race condition ???
                      val delay = overheatingDurations.next()
                      logger.debug(s"committedEventIdSync.whenAvailable(after=$after," +
                        s" timeout=${deadline.fold("")(_.timeLeft.pretty)})" +
                        s" last=${committedEventIdSync.last} => $eventArrived," +
                        s" unexpected $empty, delaying ${delay.pretty}")
                      delay
                    } else
                      ZeroDuration

                  untilNonEmpty(lastEventId).delayExecution(preventOverheating)
                } else
                  Task.pure(EventSeq.Empty(lastEventId))
              }

          case o: TearableEventSeq.Torn =>
            Task.pure(o)
        })

      untilNonEmpty(after)
  }

  private def collectEventsSince[A](
    after: EventId,
    collect: PartialFunction[AnyKeyedEvent, A],
    limit: Int)
  : TearableEventSeq[CloseableIterator, A] = {
    val last = lastAddedEventId
    if (after > last) {
      logger.debug(s"The future event requested is not yet available, " +
        s"lastAddedEventId=${EventId.toString(last)} after=${EventId.toString(after)}")
      EventSeq.Empty(last)  // Future event requested is not yet available
    } else
      eventsAfter(after) match {
        case Some(stampeds) =>
          var lastEventId = after
          val eventIterator = stampeds
            .tapEach { o => lastEventId = o.eventId }
            .collect { case stamped if collect isDefinedAt stamped.value => stamped map collect }
            .take(limit)
          if (eventIterator.isEmpty) {
            eventIterator.close()
            EventSeq.Empty(lastEventId)
          } else
            EventSeq.NonEmpty(eventIterator)
        case None =>
          TearableEventSeq.Torn(after = tornEventId)
      }
  }

  final def lastAddedEventId: EventId =
    committedEventIdSync.last

  final def checkEventId(eventId: EventId): Checked[Unit] =
    eventsAfter(eventId) match {
      case Some(iterator) =>
        iterator.close()
        Checked.unit
      case None =>
        Left(UnknownEventIdProblem(requiredEventId = eventId))
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  final def await[E <: Event: ClassTag: TypeTag](
    predicate: KeyedEvent[E] => Boolean,
    after: EventId,
    timeout: FiniteDuration)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E]]] =
    awaitAsync[E](predicate, after, timeout)
      .await(timeout + 1.s)

  @TestOnly
  final def awaitAsync[E <: Event: ClassTag: TypeTag](
    predicate: KeyedEvent[E] => Boolean,
    after: EventId,
    timeout: FiniteDuration)
    (implicit s: Scheduler)
  : Task[Vector[Stamped[KeyedEvent[E]]]] =
    when[E](EventRequest.singleClass[E](after = after, Some(timeout)), predicate)
      .map {
        case EventSeq.NonEmpty(events) =>
          try events.toVector
          finally events.close()

        case _: EventSeq.Empty =>
          throw new TimeoutException(s"RealEventWatch.await[${implicitClass[E].scalaName}]" +
            s"(after=$after, timeout=$timeout) timed out")

        //? case TearableEventSeq.Torn(tornEventId) =>
        //?   throw new TornException(after, tornEventId)

        case o =>
          sys.error(s"RealEventWatch.await[${implicitClass[E].scalaName}]" +
            s"(after=$after) unexpected EventSeq: $o")
      }
}

object RealEventWatch
{
  private val NoMoreObservable = Task.pure((None, () => throw new NoSuchElementException/*dead code*/))
  private val logger = Logger(getClass)

  private def lastOfIterator[A <: AnyRef](iterator: CloseableIterator[A]): CloseableIterator[A] = {
    var last = null.asInstanceOf[A]
    while (iterator.hasNext) last = iterator.next()
    iterator.close()
    CloseableIterator.fromIterator(Option(last).iterator)
  }
}
