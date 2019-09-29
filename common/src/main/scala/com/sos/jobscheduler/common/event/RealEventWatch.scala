package com.sos.jobscheduler.common.event

import cats.syntax.option._
import com.google.common.annotations.VisibleForTesting
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.event.RealEventWatch._
import com.sos.jobscheduler.common.scalautil.MonixUtils.closeableIteratorToObservable
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import java.util.concurrent.TimeoutException
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
trait RealEventWatch[E <: Event] extends EventWatch[E]
{
  @VisibleForTesting
  protected def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]]

  private lazy val sync = new Sync(initial = tornEventId, o => "EventId " + EventId.toString(o))  // Initialize not before whenStarted!

  protected final def onEventsAdded(eventId: EventId): Unit =
    sync.onAdded(eventId)

  final def observe[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] => Boolean): Observable[Stamped[KeyedEvent[E1]]] =
  {
    val originalTimeout = request.timeout
    var deadline = none[Deadline]

    def next(lazyRequest: () => EventRequest[E1]): Task[(Option[Observable[Stamped[KeyedEvent[E1]]]], () => EventRequest[E1])] = {
      val request = lazyRequest()  // Access now in previous iteration computed values lastEventId and limit (see below)
      deadline = request.timeout.map(t => now + (t min EventRequest.LongTimeout))  // Timeout is renewd after every fetched event
      if (request.limit <= 0)
        NoMoreObservable
      else
        when[E1](request, predicate) map {
          case TearableEventSeq.Torn(tornAfter) =>
            throw new TornException(after = request.after, tornEventId = tornAfter)

          case EventSeq.Empty(lastEventId) =>
            val remaining = deadline.map(_.timeLeft)
            (remaining.forall(_ > Duration.Zero) ?
              Observable.empty, () => request.copy[E1](after = lastEventId, timeout = deadline.map(_.timeLeftOrZero)))

          case EventSeq.NonEmpty(events) =>
            if (events.isEmpty) throw new IllegalStateException("EventSeq.NonEmpty(EMPTY)")  // Do not loop
            var lastEventId = request.after
            var limit = request.limit
            val observable = closeableIteratorToObservable(events)
              .map { o =>
                lastEventId = o.eventId
                limit -= 1
                o
              }
            // Closed-over lastEventId and limit are updated as observable is consumed, therefore defer access to final values (see above)
            (Some(observable), () => request.copy[E1](after = lastEventId, limit = limit, timeout = originalTimeout))
        }
    }
    Observable.fromAsyncStateAction(next)(() => request)
      .takeWhile(_.nonEmpty)  // Take until limit reached (NoMoreObservable) or timeout elapsed
      .map(_.get).flatten
  }

  final def read[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] => Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]] =
    request match {
      case request: EventRequest[E1] =>
        when[E1](request, predicate)
    }

  final def when[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] => Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]] =
      whenAny[E1](request, request.eventClasses, predicate)

  final def whenAny[E1 <: E](request: EventRequest[E1], eventClasses: Set[Class[_ <: E1]], predicate: KeyedEvent[E1] => Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]]
  =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if eventClasses.exists(_ isAssignableFrom e.event.getClass) && predicate(e.asInstanceOf[KeyedEvent[E1]]) =>
          e.asInstanceOf[KeyedEvent[E1]]
      })

  final def byKey[E1 <: E](request: EventRequest[E1], key: E1#Key, predicate: E1 => Boolean)
  : Task[TearableEventSeq[CloseableIterator, E1]] =
    request match {
      case request: EventRequest[E1] =>
        whenKey(request, key, predicate)
    }

  final def whenKeyedEvent[E1 <: E](request: EventRequest[E1], key: E1#Key, predicate: E1 => Boolean): Task[E1] =
    whenKey[E1](request.copy[E1](limit = 1), key, predicate) map {
      case eventSeq: EventSeq.NonEmpty[CloseableIterator, E1] =>
        try eventSeq.stamped.next().value
        finally eventSeq.close()
      case _: EventSeq.Empty => throw new TimeoutException(s"Timed out: $request")
      case _: TearableEventSeq.Torn => throw new IllegalStateException("EventSeq is torn")
    }

  final def whenKey[E1 <: E](request: EventRequest[E1], key: E1#Key, predicate: E1 => Boolean): Task[TearableEventSeq[CloseableIterator, E1]] =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if request.matchesClass(e.event.getClass) && e.key == key && predicate(e.event.asInstanceOf[E1]) =>
          e.event.asInstanceOf[E1]
      })

  private def whenAnyKeyedEvents[E1 <: E, A](request: EventRequest[E1], collect: PartialFunction[AnyKeyedEvent, A])
  : Task[TearableEventSeq[CloseableIterator, A]] = {
    val deadline = request.timeout.map(t => now + t.min(EventRequest.LongTimeout))  // Protected agains Timestamp overflow
    whenAnyKeyedEvents2(request.after, deadline, request.delay, collect, request.limit, maybeTornOlder = request.tornOlder)
  }

  private def whenAnyKeyedEvents2[A](after: EventId, deadline: Option[Deadline], delay: FiniteDuration,
    collect: PartialFunction[AnyKeyedEvent, A], limit: Int, maybeTornOlder: Option[FiniteDuration])
  : Task[TearableEventSeq[CloseableIterator, A]] =
    Task.fromFuture(whenStarted).flatMap(_ =>
      sync.whenAvailable(after, deadline, delay)
        .flatMap(_ =>
          collectEventsSince(after, collect, limit) match {
            case eventSeq @ EventSeq.NonEmpty(iterator) =>
              maybeTornOlder match {
                case None => Task.pure(eventSeq)
                case Some(tornOlder) =>
                  // If the first event is not fresh, we have a read congestion.
                  // We serve a (simulated) Torn, and the client can fetch the current state and read fresh events, skipping the congestion..
                  val head = iterator.next()
                  if (EventId.toTimestamp(head.eventId) + tornOlder < Timestamp.now) {  // Don't compare head.timestamp, timestamp may be much older)
                    iterator.close()
                    Task.pure(TearableEventSeq.Torn(sync.last))  // Simulate a torn EventSeq
                  } else
                    Task.pure(EventSeq.NonEmpty(head +: iterator))
              }

            case EventSeq.Empty(lastEventId) =>
              if (deadline.forall(_.hasTimeLeft))
                whenAnyKeyedEvents2(lastEventId, deadline, delay, collect, limit, maybeTornOlder)
              else
                Task.pure(EventSeq.Empty(lastEventId))

            case o: TearableEventSeq.Torn =>
              Task.pure(o)
          }))

  private def collectEventsSince[A](after: EventId, collect: PartialFunction[AnyKeyedEvent, A], limit: Int)
  : TearableEventSeq[CloseableIterator, A] =
    eventsAfter(after) match {
      case Some(stampeds) =>
        var lastEventId = after
        val eventIterator = stampeds
          .map { o => lastEventId = o.eventId; o }
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

  def lastAddedEventId: EventId =
    sync.last

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E1 <: E: ClassTag: TypeTag](predicate: KeyedEvent[E1] => Boolean, after: EventId, timeout: FiniteDuration)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E1]]] =
    when[E1](EventRequest.singleClass[E1](after = after, Some(timeout)), predicate) await timeout + 1.seconds match {
      case EventSeq.NonEmpty(events) =>
        try events.toVector
        finally events.close()

      case _: EventSeq.Empty =>
        throw new TimeoutException(s"RealEventWatch.await[${implicitClass[E1].scalaName}](after=$after, timeout=$timeout) timed out")

      case o =>
        sys.error(s"RealEventWatch.await[${implicitClass[E1].scalaName}](after=$after) unexpected EventSeq: $o")
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E1 <: E: ClassTag: TypeTag](implicit s: Scheduler): TearableEventSeq[CloseableIterator, KeyedEvent[E1]] =
    when[E1](EventRequest.singleClass(), _ => true) await 99.s
}

object RealEventWatch {
  private val NoMoreObservable = Task.pure((None, () => throw new NoSuchElementException/*dead code*/))
}
