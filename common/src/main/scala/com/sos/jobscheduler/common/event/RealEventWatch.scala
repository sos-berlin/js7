package com.sos.jobscheduler.common.event

import com.google.common.annotations.VisibleForTesting
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import com.sos.jobscheduler.common.akkahttp.StreamingSupport.closeableIteratorToObservable
import com.sos.jobscheduler.common.event.RealEventWatch._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, ReverseEventRequest, SomeEventRequest, Stamped, TearableEventSeq}
import java.util.concurrent.TimeoutException
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait RealEventWatch[E <: Event] extends EventWatch[E]
{
  protected def timerService: TimerService

  def whenStarted: Task[this.type] = Task.pure(this)

  @VisibleForTesting
  protected def eventsAfter(after: EventId): Option[CloseableIterator[Stamped[KeyedEvent[E]]]]

  protected def reverseEventsAfter(after: EventId): CloseableIterator[Stamped[KeyedEvent[E]]]

  private lazy val sync = new Sync(initialLastEventId = tornEventId, timerService)  // Initialize not before whenStarted!

  protected final def onEventsAdded(eventId: EventId): Unit =
    sync.onEventAdded(eventId)

  final def observe[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean): Observable[Stamped[KeyedEvent[E1]]] =
  {
    def next(lazyRequest: () ⇒ EventRequest[E1]): Task[(Option[Observable[Stamped[KeyedEvent[E1]]]], () ⇒ EventRequest[E1])] = {
      val request = lazyRequest()  // Access now in previous iteration computed values lastEventId and limit (see below)
      if (request.limit <= 0)
        NoMoreObservable
      else
        when[E1](request, predicate) map {
          case _: TearableEventSeq.Torn ⇒
            throw new TornException(after = request.after, tornEventId = tornEventId)

          case EventSeq.Empty(lastEventId) ⇒
            (Some(Observable.empty), () ⇒ request.copy[E1](after = lastEventId))

          case EventSeq.NonEmpty(events) ⇒
            if (events.isEmpty) throw new IllegalStateException("EventSeq.NonEmpty(EMPTY)")  // Do not loop
            var lastEventId = request.after
            var limit = request.limit
            val observable = closeableIteratorToObservable(events)
              .map { o ⇒
                lastEventId = o.eventId
                limit -= 1
                o
              }
            // Closed-over lastEventId and limit are updated as observable is consumed, therefore defer access to final values
            (Some(observable), () ⇒ request.copy[E1](after = lastEventId, limit = limit, timeout = Duration.Zero))
        }
    }
    Observable.fromAsyncStateAction(next)(() ⇒ request).takeWhile(_.nonEmpty).map(_.get).flatten
  }

  final def read[E1 <: E](request: SomeEventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]] =
    request match {
      case request: EventRequest[E1] ⇒
        when[E1](request, predicate)

      case request: ReverseEventRequest[E1] ⇒
        Task(reverse[E1](request, predicate))
    }

  final def when[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]] =
      whenAny[E1](request, request.eventClasses, predicate)

  final def whenAny[E1 <: E](request: EventRequest[E1], eventClasses: Set[Class[_ <: E1]], predicate: KeyedEvent[E1] ⇒ Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]]
  =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if eventClasses.exists(_ isAssignableFrom e.event.getClass) && predicate(e.asInstanceOf[KeyedEvent[E1]]) ⇒
          e.asInstanceOf[KeyedEvent[E1]]
      })

  final def byKey[E1 <: E](request: SomeEventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean)
  : Task[TearableEventSeq[CloseableIterator, E1]] =
    request match {
      case request: EventRequest[E1] ⇒
        whenKey(request, key, predicate)
      case request: ReverseEventRequest[E1] ⇒
        Task(reverseForKey(request, key))
    }

  final def whenKeyedEvent[E1 <: E](request: EventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean): Task[E1] =
    whenKey[E1](request.copy[E1](limit = 1), key, predicate) map {
      case eventSeq: EventSeq.NonEmpty[CloseableIterator, E1] ⇒
        try eventSeq.stamped.next().value
        finally eventSeq.close()
      case _: EventSeq.Empty ⇒ throw new TimeoutException(s"Timed out: $request")
      case _: TearableEventSeq.Torn ⇒ throw new IllegalStateException("EventSeq is torn")
    }

  final def whenKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 ⇒ Boolean)
  : Task[TearableEventSeq[CloseableIterator, E1]]
  =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if request.matchesClass(e.event.getClass) && e.key == key && predicate(e.event.asInstanceOf[E1]) ⇒
          e.event.asInstanceOf[E1]
      })

  private def whenAnyKeyedEvents[E1 <: E, A](request: EventRequest[E1], collect: PartialFunction[AnyKeyedEvent, A])
  : Task[TearableEventSeq[CloseableIterator, A]] = {
    val until = now + (request.timeout min 365.days)  // Protected agains Timestamp overflow
    whenAnyKeyedEvents2(request.after, until, request.delay, collect, request.limit)
  }

  private def whenAnyKeyedEvents2[A](after: EventId, until: Timestamp, delay: FiniteDuration, collect: PartialFunction[AnyKeyedEvent, A], limit: Int)
  : Task[TearableEventSeq[CloseableIterator, A]] =
    whenStarted.flatMap (_ ⇒
      Task.deferFutureAction(implicit s ⇒
        sync.whenEventIsAvailable(after, until, delay))
        .flatMap (_ ⇒
          collectEventsSince(after, collect, limit) match {
            case o @ EventSeq.NonEmpty(_) ⇒
              Task.pure(o)
            case EventSeq.Empty(lastEventId) if now < until ⇒
              whenAnyKeyedEvents2(lastEventId, until, delay, collect, limit)
            case EventSeq.Empty(lastEventId) ⇒
              Task.pure(EventSeq.Empty(lastEventId))
            case o: TearableEventSeq.Torn ⇒
              Task.pure(o)
          }))

  private def collectEventsSince[A](after: EventId, collect: PartialFunction[AnyKeyedEvent, A], limit: Int)
  : TearableEventSeq[CloseableIterator, A] =
    eventsAfter(after) match {
      case Some(stampeds) ⇒
        var lastEventId = after
        val eventIterator = stampeds
          .map { o ⇒ lastEventId = o.eventId; o }
          .collect { case stamped if collect isDefinedAt stamped.value ⇒ stamped map collect }
          .take(limit)
        if (eventIterator.isEmpty) {
          eventIterator.close()
          EventSeq.Empty(lastEventId)
        } else
          EventSeq.NonEmpty(eventIterator)
      case None ⇒
        TearableEventSeq.Torn(after = tornEventId)
      }

  protected final def reverse[E1 <: E](request: ReverseEventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean)
  : EventSeq[CloseableIterator, KeyedEvent[E1]] =
    reverseEventsAfter(after = request.after)
      .collect {
        case stamped if request matchesClass stamped.value.event.getClass ⇒
          stamped.asInstanceOf[Stamped[KeyedEvent[E1]]]
      }
      .filter(stamped ⇒ predicate(stamped.value))
      .take(request.limit) match {
        case stampeds if stampeds.nonEmpty ⇒ EventSeq.NonEmpty(stampeds)
        case _ ⇒ EventSeq.Empty(lastAddedEventId)
      }

  protected final def reverseForKey[E1 <: E](request: ReverseEventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean = (_: E1) ⇒ true)
  : EventSeq.NonEmpty[CloseableIterator, E1] =
    EventSeq.NonEmpty(
      reverseEventsAfter(after = request.after).collect {
        case stamped if request matchesClass stamped.value.event.getClass ⇒
          stamped.asInstanceOf[Stamped[KeyedEvent[E1]]]
      }
      .collect {
        case Stamped(eventId, timestamp, KeyedEvent(`key`, event)) if predicate(event) ⇒
          Stamped(eventId, timestamp, event)
      }
      .take(request.limit))

  def lastAddedEventId: EventId =
    sync.lastAddedEventId

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E1 <: E: ClassTag](predicate: KeyedEvent[E1] ⇒ Boolean, after: EventId, timeout: FiniteDuration)(implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E1]]] =
    when[E1](EventRequest.singleClass[E1](after = after, timeout), predicate) await timeout + 1.seconds match {
      case EventSeq.NonEmpty(events) ⇒
        try events.toVector
        finally events.close()

      case _: EventSeq.Empty ⇒
        sys.error(s"RealEventWatch.await[${implicitClass[E1].scalaName}](after=$after, timeout=$timeout) timed out")

      case o ⇒
        sys.error(s"RealEventWatch.await[${implicitClass[E1].scalaName}](after=$after) unexpected EventSeq: $o")
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E1 <: E: ClassTag](implicit s: Scheduler): TearableEventSeq[CloseableIterator, KeyedEvent[E1]] =
    when[E1](EventRequest.singleClass(), _ ⇒ true) await 99.s
}

object RealEventWatch {
  private val NoMoreObservable = Task.pure((None, () ⇒ throw new NoSuchElementException/*dead code*/))

  final class TornException private[RealEventWatch](val after: EventId, val tornEventId: EventId)
  extends RuntimeException {
    override def getMessage = s"EventSeq is torn - after=$after tornEventId=$tornEventId"
  }
}
