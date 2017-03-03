package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.common.event.collector.EventCollector._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.RichFutureFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, ReverseEventRequest, SomeEventRequest, Stamped, TearableEventSeq}
import com.typesafe.config.Config
import java.time.Instant.now
import java.time.{Duration, Instant}
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
abstract class EventCollector(initialOldestEventId: EventId, configuration: Configuration)
  (implicit
    timerService: TimerService,
    executionContext: ExecutionContext)
{
  private[collector] val keyedEventQueue = new KeyedEventQueue(initialOldestEventId = initialOldestEventId, sizeLimit = configuration.queueSize)
  private val sync = new Sync(initialLastEventId = initialOldestEventId, timerService)

  logger.debug("oldestEventId=" + EventId.toString(oldestEventId))

  final def addStamped(stamped: Stamped[AnyKeyedEvent]): Unit = {
    keyedEventQueue.add(stamped)
    sync.onNewEvent(stamped.eventId)
  }

  final def byPredicate[E <: Event](request: SomeEventRequest[E], predicate: KeyedEvent[E] ⇒ Boolean): Future[TearableEventSeq[Iterator, KeyedEvent[E]]] =
    request match {
      case request: EventRequest[E] ⇒
        when[E](request, predicate)
      case request: ReverseEventRequest[E] ⇒
        val stampeds = reverse[E](request, predicate)
        Future.successful(
          if (stampeds.nonEmpty)
            EventSeq.NonEmpty(stampeds)
          else
            EventSeq.Empty(keyedEventQueue.lastEventId))  // eventReverse is only for testing purposes
  }

  final def when[E <: Event](
    request: EventRequest[E],
    predicate: KeyedEvent[E] ⇒ Boolean = (_: KeyedEvent[E]) ⇒ true)
  : Future[TearableEventSeq[Iterator, KeyedEvent[E]]]
  =
    whenAny[E](request, request.eventClasses, predicate)

  final def whenAny[E <: Event](
    request: EventRequest[E],
    eventClasses: Set[Class[_ <: E]],
    predicate: KeyedEvent[E] ⇒ Boolean = (_: KeyedEvent[E]) ⇒ true)
  : Future[TearableEventSeq[Iterator, KeyedEvent[E]]]
  =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if eventClasses.exists(_ isAssignableFrom e.event.getClass) && predicate(e.asInstanceOf[KeyedEvent[E]]) ⇒
          e.asInstanceOf[KeyedEvent[E]]
      })

  final def byKeyAndPredicate[E <: Event](request: SomeEventRequest[E], key: E#Key, predicate: E ⇒ Boolean): Future[TearableEventSeq[Iterator, E]] =
    request match {
      case request: EventRequest[E] ⇒
        whenForKey(request, key, predicate)
      case request: ReverseEventRequest[E] ⇒
        Future.successful(EventSeq.NonEmpty(reverseForKey(request, key)))
    }

  final def whenForKey[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E ⇒ Boolean = (_: E) ⇒ true)
  : Future[TearableEventSeq[Iterator, E]]
  =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if request.matchesClass(e.event.getClass) && e.key == key && predicate(e.event.asInstanceOf[E]) ⇒
          e.event.asInstanceOf[E]
      })

  private def whenAnyKeyedEvents[E <: Event, A](request: EventRequest[E], collect: PartialFunction[AnyKeyedEvent, A]): Future[TearableEventSeq[Iterator, A]] =
    whenAnyKeyedEvents2(request.after, now + (request.timeout min configuration.timeoutLimit), collect, request.limit)

  private def whenAnyKeyedEvents2[A](after: EventId, until: Instant, collect: PartialFunction[AnyKeyedEvent, A], limit: Int): Future[TearableEventSeq[Iterator, A]] =
    (for (_ ← sync.whenEventIsAvailable(after, until)) yield
      collectEventsSince(after, collect, limit) match {
        case o @ EventSeq.NonEmpty(_) ⇒
          Future.successful(o)
        case EventSeq.Empty(lastEventId) if now < until ⇒
          whenAnyKeyedEvents2(lastEventId, until, collect, limit)
        case EventSeq.Empty(lastEventId) ⇒
          Future.successful(EventSeq.Empty(lastEventId))
        case EventSeq.Torn ⇒
          Future.successful(EventSeq.Torn)
    }).flatten

  private def collectEventsSince[A](after: EventId, collect: PartialFunction[AnyKeyedEvent, A], limit: Int): TearableEventSeq[Iterator, A] = {
    require(limit >= 0, "limit must be >= 0")
    val stampedsOption = keyedEventQueue.after(after)
    var lastEventId = after
    stampedsOption match {
      case Some(stampeds) ⇒
        val eventIterator =
          stampeds
            .map { o ⇒ lastEventId = o.eventId; o }
            .collect {
              case stamped if collect.isDefinedAt(stamped.value) ⇒
                Stamped(stamped.eventId, collect(stamped.value))
            }
            .take(limit)
        if (eventIterator.nonEmpty)
          EventSeq.NonEmpty(eventIterator)
        else
          EventSeq.Empty(lastEventId)
      case None ⇒
        EventSeq.Torn
    }
  }

  final def reverse[E <: Event](
    request: ReverseEventRequest[E],
    predicate: KeyedEvent[E] ⇒ Boolean = (_: KeyedEvent[E]) ⇒ true)
  : Iterator[Stamped[KeyedEvent[E]]]
  =
    keyedEventQueue.reverseEvents(after = request.after)
      .collect {
        case stamped if request matchesClass stamped.value.event.getClass ⇒
          stamped.asInstanceOf[Stamped[KeyedEvent[E]]]
      }
      .filter(stamped ⇒ predicate(stamped.value))
      .take(request.limit)

  final def reverseForKey[E <: Event](
    request: ReverseEventRequest[E],
    key: E#Key,
    predicate: E ⇒ Boolean = (_: E) ⇒ true)
  : Iterator[Stamped[E]]
  =
    keyedEventQueue.reverseEvents(after = request.after)
      .collect {
        case stamped if request matchesClass stamped.value.event.getClass ⇒
          stamped.asInstanceOf[Stamped[KeyedEvent[E]]]
      }
      .collect {
        case Stamped(eventId, KeyedEvent(`key`, event)) if predicate(event) ⇒
          Stamped(eventId, event)
      }
      .take(request.limit)

  final def oldestEventId: EventId =
    keyedEventQueue.oldestEventId

  final def lastEventId: EventId =
    keyedEventQueue.lastEventId

  final def eventCount: Int =
    keyedEventQueue.size
}

object EventCollector {
  private val logger = Logger(getClass)

  final case class Configuration(
    queueSize: Int,
    /** Limits open requests, and avoids arithmetic overflow. */
    timeoutLimit: Duration)

  object Configuration {
    val ForTest = Configuration(queueSize = 1000, timeoutLimit = 600.s)

    final def fromSubConfig(config: Config) = Configuration(
      queueSize = config.getInt("queue-size"),
      timeoutLimit = config.getDuration("timeout-limit")
    )
  }
}
