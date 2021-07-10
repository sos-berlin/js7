package js7.journal.watch

import cats.syntax.option._
import java.util.concurrent.TimeoutException
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.closeableIteratorToObservable
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
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
  @TestOnly
  protected def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[Event]]]]

  protected def isActiveNode: Boolean

  // Lazy, initialize only after whenStarted has been called!
  private lazy val committedEventIdSync = new EventSync(initial = tornEventId, o => "EventId " + EventId.toString(o))

  protected final def onEventsCommitted(eventId: EventId): Unit =
    committedEventIdSync.onAdded(eventId)

  final def observe[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean, onlyAcks: Boolean)
  : Observable[Stamped[KeyedEvent[E]]] =
  {
    val originalTimeout = request.timeout
    var deadline = none[MonixDeadline]

    def next(lazyRequest: () => EventRequest[E]): Task[(Option[Observable[Stamped[KeyedEvent[E]]]], () => EventRequest[E])] =
      Task.deferAction { implicit s =>
        val request = lazyRequest()  // Access now in previous iteration computed values lastEventId and limit (see below)
        deadline = request.timeout.map(t => now + (t min EventRequest.LongTimeout))  // Timeout is renewed after every fetched event
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
              // Closed-over lastEventId and limit are updated as observable is consumed, therefore defer access to final values (see above)
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

    def next(lazyAfter: () => (EventId, Option[FiniteDuration])): Task[(Option[Observable[EventId]], () => (EventId, Option[FiniteDuration]))] =
      Task.deferAction { implicit s =>
        val (after, maybeTimeout) = lazyAfter()  // Access now in previous iteration computed values lastEventId and limit (see below)
        deadline = maybeTimeout.map(t => now + (t min EventRequest.LongTimeout))  // Timeout is renewed after every fetched event
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
              Task.raiseError(
                Problem.pure("This active cluster node does not provide event acknowledgements (two active cluster nodes?)")
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

  final def read[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]] =
    request match {
      case request: EventRequest[E] =>
        when[E](request, predicate)
    }

  final def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]] =
      whenAny[E](request, request.eventClasses, predicate)

  final def whenAny[E <: Event](request: EventRequest[E], eventClasses: Set[Class[_ <: E]], predicate: KeyedEvent[E] => Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]
  =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if eventClasses.exists(_ isAssignableFrom e.event.getClass) && predicate(e.asInstanceOf[KeyedEvent[E]]) =>
          e.asInstanceOf[KeyedEvent[E]]
      })

  final def byKey[E <: Event](request: EventRequest[E], key: E#Key, predicate: E => Boolean)
  : Task[TearableEventSeq[CloseableIterator, E]] =
    request match {
      case request: EventRequest[E] =>
        whenKey(request, key, predicate)
    }

  final def whenKeyedEvent[E <: Event](request: EventRequest[E], key: E#Key, predicate: E => Boolean): Task[E] =
    whenKey[E](request.copy[E](limit = 1), key, predicate) map {
      case eventSeq: EventSeq.NonEmpty[CloseableIterator, E] =>
        try eventSeq.stamped.next().value
        finally eventSeq.close()
      case _: EventSeq.Empty => throw new TimeoutException(s"Timed out: $request")
      case _: TearableEventSeq.Torn => throw new IllegalStateException("EventSeq is torn")
    }

  final def whenKey[E <: Event](request: EventRequest[E], key: E#Key, predicate: E => Boolean): Task[TearableEventSeq[CloseableIterator, E]] =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if request.matchesClass(e.event.getClass) && e.key == key && predicate(e.event.asInstanceOf[E]) =>
          e.event.asInstanceOf[E]
      })

  private def whenAnyKeyedEvents[E <: Event, A](request: EventRequest[E], collect: PartialFunction[AnyKeyedEvent, A])
  : Task[TearableEventSeq[CloseableIterator, A]] = Task.deferAction { implicit s =>
    val deadline = request.timeout.map(t => now + t.min(EventRequest.LongTimeout))  // Protected agains Timestamp overflow
    whenAnyKeyedEvents2(request.after, deadline, request.delay, collect, request.limit, maybeTornOlder = request.tornOlder)
  }

  private def whenAnyKeyedEvents2[A](after: EventId, deadline: Option[MonixDeadline], delay: FiniteDuration,
    collect: PartialFunction[AnyKeyedEvent, A], limit: Int, maybeTornOlder: Option[FiniteDuration])
  : Task[TearableEventSeq[CloseableIterator, A]] =
    Task.fromFuture(whenStarted)
      .flatMap(_ => committedEventIdSync.whenAvailable(after, deadline, delay))
      .flatMap(eventArrived =>
        collectEventsSince(after, collect, limit) match {
          case eventSeq @ EventSeq.NonEmpty(iterator) =>
            maybeTornOlder match {
              case None => Task.pure(eventSeq)
              case Some(tornOlder) =>
                // If the first event is not fresh, we have a read congestion.
                // We serve a (simulated) Torn, and the client can fetch the current state and read fresh events, skipping the congestion..
                Task.deferAction { implicit s =>
                  val head = iterator.next()
                  if (EventId.toTimestamp(head.eventId) + tornOlder < Timestamp.now) {  // Don't compare head.timestamp, timestamp may be much older)
                    iterator.close()
                    Task.pure(TearableEventSeq.Torn(committedEventIdSync.last))  // Simulate a torn EventSeq
                  } else
                    Task.pure(EventSeq.NonEmpty(head +: iterator))
                }
            }

          case empty @ EventSeq.Empty(lastEventId) =>
            Task.deferAction { implicit s =>
              if (deadline.forall(_.hasTimeLeft)) {
                val preventOverheating =
                  if (lastEventId < committedEventIdSync.last) {
                    // May happen due to race condition?
                    logger.debug(s"committedEventIdSync.whenAvailable(after=$after," +
                      s" timeout=${deadline.fold("")(_.timeLeft.pretty)}) last=${committedEventIdSync.last} => $eventArrived," +
                      s" unexpected $empty, delaying ${AvoidOverheatingDelay.pretty}")
                    AvoidOverheatingDelay
                  } else
                    ZeroDuration
                whenAnyKeyedEvents2(lastEventId, deadline, delay, collect, limit, maybeTornOlder)
                  .delayExecution(preventOverheating)
              } else
                Task.pure(EventSeq.Empty(lastEventId))
            }

        case o: TearableEventSeq.Torn =>
          Task.pure(o)
      })

  private def collectEventsSince[A](after: EventId, collect: PartialFunction[AnyKeyedEvent, A], limit: Int)
  : TearableEventSeq[CloseableIterator, A] = {
    val last = lastAddedEventId
    if (after > last) {
      logger.debug(s"The future event requested is not yet available, lastAddedEventId=${EventId.toString(last)} after=${EventId.toString(after)}")
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

  def lastAddedEventId: EventId =
    committedEventIdSync.last

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E <: Event: ClassTag: TypeTag](predicate: KeyedEvent[E] => Boolean, after: EventId, timeout: FiniteDuration)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E]]] =
    when[E](EventRequest.singleClass[E](after = after, Some(timeout)), predicate) await timeout + 1.s match {
      case EventSeq.NonEmpty(events) =>
        try events.toVector
        finally events.close()

      case _: EventSeq.Empty =>
        throw new TimeoutException(s"RealEventWatch.await[${implicitClass[E].scalaName}](after=$after, timeout=$timeout) timed out")

      //? case TearableEventSeq.Torn(tornEventId) =>
      //?   throw new TornException(after, tornEventId)

      case o =>
        sys.error(s"RealEventWatch.await[${implicitClass[E].scalaName}](after=$after) unexpected EventSeq: $o")
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E <: Event: ClassTag: TypeTag](after: EventId)(implicit s: Scheduler): TearableEventSeq[CloseableIterator, KeyedEvent[E]] =
    when[E](EventRequest.singleClass[E](after = after), _ => true) await 99.s
}

object RealEventWatch
{
  private val AvoidOverheatingDelay = 10.ms
  private val NoMoreObservable = Task.pure((None, () => throw new NoSuchElementException/*dead code*/))
  private val logger = Logger(getClass)

  private def lastOfIterator[A <: AnyRef](iterator: CloseableIterator[A]): CloseableIterator[A] = {
    var last = null.asInstanceOf[A]
    while (iterator.hasNext) last = iterator.next()
    iterator.close()
    CloseableIterator.fromIterator(Option(last).iterator)
  }
}
