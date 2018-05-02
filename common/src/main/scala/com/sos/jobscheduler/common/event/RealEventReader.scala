package com.sos.jobscheduler.common.event

import com.google.common.annotations.VisibleForTesting
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import com.sos.jobscheduler.common.akkahttp.StreamingSupport.closeableIteratorToObservable
import com.sos.jobscheduler.common.event.RealEventReader._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, ReverseEventRequest, SomeEventRequest, Stamped, TearableEventSeq}
import java.util.concurrent.TimeoutException
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait RealEventReader[E <: Event] extends EventReader[E]
{
  protected def timerService: TimerService

  protected def timeoutLimit: Duration

  protected def oldestEventId: EventId

  @VisibleForTesting
  def eventsAfter(after: EventId): Task[Option[CloseableIterator[Stamped[KeyedEvent[E]]]]]

  protected def reverseEventsAfter(after: EventId): Task[CloseableIterator[Stamped[KeyedEvent[E]]]]

  private var _lastEventId: EventId = EventId.BeforeFirst
  private lazy val sync = new Sync(initialLastEventId = oldestEventId, timerService)

  final val whenRealEventReader = Future.successful(this)

  protected final def onEventAdded(eventId: EventId): Unit = {
    if (eventId < _lastEventId) throw new IllegalArgumentException(s"RealEventReader: Added EventId ${EventId.toString(eventId)} < last EventId ${EventId.toString(_lastEventId)}")
    _lastEventId = eventId
    sync.onEventAdded(eventId)
  }

  final def observe[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean)
    (implicit scheduler: Scheduler)
  : Observable[Stamped[KeyedEvent[E1]]] =
    recursive_observe2(request, predicate, limitRecursion = 100/*2000 would blow up test*/)
    //tailRecM_observe(request, predicate)

  // Blows up stack when not limited
  private def recursive_observe2[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean, limitRecursion: Int)
    (implicit scheduler: Scheduler)
  : Observable[Stamped[KeyedEvent[E1]]] =
  {
    if (request.limit <= 0 || limitRecursion <= 0)
      Observable.empty
    else
      Observable.fromTask(read[E1](request, predicate = predicate)) flatMap {
        case _: TearableEventSeq.Torn ⇒
          throw new IllegalArgumentException(s"EventSeq is torn - after=${request.after} oldestEventId=$oldestEventId")

        case EventSeq.Empty(lastEventId) ⇒
          val nextRequest = request.copy[E1](after = lastEventId)
          recursive_observe2(nextRequest, predicate, limitRecursion - 1)

        case EventSeq.NonEmpty(events) ⇒
          if (events.isEmpty) throw new IllegalStateException("EventSeq.NonEmpty(EMPTY)")  // Do not loop
          var lastEventId = request.after
          var limit = request.limit
          closeableIteratorToObservable(events)
            .map { o ⇒
              lastEventId = o.eventId
              limit -= 1
              o
            } ++/*<-- Monix 3.0 concatention will not expand stack ??? */
            Observable.defer(
              recursive_observe2(request.copy[E1](after = lastEventId, limit = limit), predicate, limitRecursion - 1))
      }
    }

  // This implementations blows up heap. See Alexandru's notes in https://github.com/typelevel/cats/issues/1329
  private def tailRecM_observe[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean)
    (implicit scheduler: Scheduler)
  =
    Observable.tailRecM((request, predicate)) { case (req, pred) ⇒
      if (req.limit <= 0)
        Observable.empty
      else
        Observable.fromTask(read[E1](req, predicate = pred)) flatMap {
          case _: TearableEventSeq.Torn ⇒
            throw new IllegalArgumentException(s"EventSeq is torn - after=${req.after} oldestEventId=$oldestEventId")

          case EventSeq.Empty(lastEventId) ⇒
            Observable(Left((req.copy[E1](after = lastEventId), pred)))

          case EventSeq.NonEmpty(events) ⇒
            if (events.isEmpty) throw new IllegalStateException("EventSeq.NonEmpty(EMPTY)")  // Do not loop
            var lastEventId = req.after
            var limit = req.limit
            closeableIteratorToObservable(events)
              .map(Right.apply)
              .endWith(Left(PlaceHolder) :: Nil)
              .map {
                case Right(stamped) ⇒
                  lastEventId = stamped.eventId
                  limit -= 1
                  Right(stamped)
                case Left(PlaceHolder) ⇒
                  Left((req.copy[E1](after = lastEventId, limit = limit), pred))
              }
        }
    }

  final def read[E1 <: E](request: SomeEventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]] =
    request match {
      case request: EventRequest[E1] ⇒
        when[E1](request, predicate)
      case request: ReverseEventRequest[E1] ⇒
        reverse[E1](request, predicate)
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
        reverseForKey(request, key)
    }

  final def whenKeyedEvent[E1 <: E](request: EventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean): Task[E1] =
    whenKey[E1](request.copy[E1](limit = 1), key, predicate) map {
      case eventSeq: EventSeq.NonEmpty[CloseableIterator, E1] ⇒
        try eventSeq.stampeds.next().value
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
  : Task[TearableEventSeq[CloseableIterator, A]] =
    whenAnyKeyedEvents2(request.after, now + (request.timeout min timeoutLimit), request.delay, collect, request.limit)

  private def whenAnyKeyedEvents2[A](after: EventId, until: Timestamp, delay: FiniteDuration, collect: PartialFunction[AnyKeyedEvent, A], limit: Int)
  : Task[TearableEventSeq[CloseableIterator, A]] =
    Task.deferFutureAction(implicit s ⇒ sync.whenEventIsAvailable(after, until, delay))
      .flatMap (_ ⇒
        collectEventsSince(after, collect, limit) flatMap {
          case o @ EventSeq.NonEmpty(_) ⇒
            Task.pure(o)
          case EventSeq.Empty(lastEventId) if now < until ⇒
            whenAnyKeyedEvents2(lastEventId, until, delay, collect, limit)
          case EventSeq.Empty(lastEventId) ⇒
            Task.pure(EventSeq.Empty(lastEventId))
          case o: TearableEventSeq.Torn ⇒
            Task.pure(o)
        })

  private def collectEventsSince[A](after: EventId, collect: PartialFunction[AnyKeyedEvent, A], limit: Int)
  : Task[TearableEventSeq[CloseableIterator, A]] = {
    require(limit >= 0, "limit must be >= 0")
    for (stampedsOption ← eventsAfter(after)) yield
      stampedsOption match {
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
          TearableEventSeq.Torn(oldestKnownEventId = oldestEventId)
      }
  }

  protected final def reverse[E1 <: E](request: ReverseEventRequest[E1], predicate: KeyedEvent[E1] ⇒ Boolean)
  : Task[EventSeq[CloseableIterator, KeyedEvent[E1]]] =
    reverseEventsAfter(after = request.after).map(_
      .collect {
        case stamped if request matchesClass stamped.value.event.getClass ⇒
          stamped.asInstanceOf[Stamped[KeyedEvent[E1]]]
      }
      .filter(stamped ⇒ predicate(stamped.value))
      .take(request.limit) match {
        case stampeds if stampeds.nonEmpty ⇒ EventSeq.NonEmpty(stampeds)
        case _ ⇒ EventSeq.Empty(lastAddedEventId)
      })

  protected final def reverseForKey[E1 <: E](request: ReverseEventRequest[E1], key: E1#Key, predicate: E1 ⇒ Boolean = (_: E1) ⇒ true)
  : Task[EventSeq.NonEmpty[CloseableIterator, E1]] =
    for (stampeds ← reverseEventsAfter(after = request.after)) yield
      EventSeq.NonEmpty(
        stampeds.collect {
          case stamped if request matchesClass stamped.value.event.getClass ⇒
            stamped.asInstanceOf[Stamped[KeyedEvent[E1]]]
        }
        .collect {
          case Stamped(eventId, timestamp, KeyedEvent(`key`, event)) if predicate(event) ⇒
            Stamped(eventId, timestamp, event)
        }
        .take(request.limit))

  def lastAddedEventId: EventId =
    if (_lastEventId != EventId.BeforeFirst) _lastEventId else oldestEventId

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E1 <: E: ClassTag](predicate: KeyedEvent[E1] ⇒ Boolean, after: EventId, timeout: FiniteDuration)(implicit s: Scheduler) =
    when[E1](EventRequest.singleClass[E1](after = after, timeout), predicate) await timeout + 1.seconds match {
      case EventSeq.NonEmpty(events) ⇒
        try events.toVector
        finally events.close()
      case o ⇒ sys.error(s"RealEventReader.await[${implicitClass[E1].scalaName}] unexpected EventSeq: $o")
    }

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E1 <: E: ClassTag](implicit s: Scheduler): TearableEventSeq[CloseableIterator, KeyedEvent[E1]] =
    when[E1](EventRequest.singleClass(after = EventId.BeforeFirst, timeout = 0.seconds), _ ⇒ true) await 99.s
}

object RealEventReader {
  private object PlaceHolder
}
