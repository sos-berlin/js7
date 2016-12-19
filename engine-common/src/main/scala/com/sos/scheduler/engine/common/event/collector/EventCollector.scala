package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.event.EventIdGenerator
import com.sos.scheduler.engine.common.event.collector.EventCollector._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.RichFutureFuture
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, ReverseEventRequest, Snapshot, SomeEventRequest}
import com.typesafe.config.Config
import java.time.Instant.now
import java.time.{Duration, Instant}
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}
import scala.math.max

/**
  * @author Joacim Zschimmer
  */
trait EventCollector {

  protected val configuration: Configuration
  protected val eventIdGenerator: EventIdGenerator
  protected val timerService: TimerService
  protected implicit val executionContext: ExecutionContext

  private[collector] val keyedEventQueue = new KeyedEventQueue(sizeLimit = configuration.queueSize)
  //private val keyToEventQueue = new concurrent.TrieMap[Any, EventQueue]()

  private val sync = new Sync(timerService)

  protected final def putEvent(keyedEvent: AnyKeyedEvent): Unit = {
    //keyToEventQueue.getOrElseUpdate(keyedEvent.key, new EventQueue(EventQueueSizeLimitPerKey))
    //  .add(Snapshot(eventId, keyedEvent.event))
    val eventId = eventIdGenerator.next()
    keyedEventQueue.add(Snapshot(eventId, keyedEvent))
    sync.onNewEvent(eventId)
  }

  def byPredicate[E <: Event](request: SomeEventRequest[E], predicate: KeyedEvent[E] ⇒ Boolean): Future[EventSeq[Iterator, KeyedEvent[E]]] =
    request match {
      case request: EventRequest[E] ⇒
        when[E](request, predicate)
      case request: ReverseEventRequest[E] ⇒
        val snapshots = reverse[E](request, predicate)
        Future.successful(
          if (snapshots.nonEmpty)
            EventSeq.NonEmpty(snapshots)
          else
            EventSeq.Empty(eventIdGenerator.lastUsedEventId))  // eventReverse is only for testing purposes
  }

  final def when[E <: Event](
    request: EventRequest[E],
    predicate: KeyedEvent[E] ⇒ Boolean = (_: KeyedEvent[E]) ⇒ true)
  : Future[EventSeq[Iterator, KeyedEvent[E]]]
  =
    whenAny[E](request, Set[Class[_ <: E]](request.eventClass), predicate)

  final def whenAny[E <: Event](
    request: EventRequest[E],
    eventClasses: Set[Class[_ <: E]],
    predicate: KeyedEvent[E] ⇒ Boolean = (_: KeyedEvent[E]) ⇒ true)
  : Future[EventSeq[Iterator, KeyedEvent[E]]]
  =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if eventClasses.exists(_ isAssignableFrom e.event.getClass) && predicate(e.asInstanceOf[KeyedEvent[E]]) ⇒
          e.asInstanceOf[KeyedEvent[E]]
      })

  def byKeyAndPredicate[E <: Event](request: SomeEventRequest[E], key: E#Key, predicate: E ⇒ Boolean): Future[EventSeq[Iterator, E]] =
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
  : Future[EventSeq[Iterator, E]]
  =
    whenAnyKeyedEvents(
      request,
      collect = {
        case e if (request.eventClass isAssignableFrom e.event.getClass) && e.key == key && predicate(e.event.asInstanceOf[E]) ⇒
          e.event.asInstanceOf[E]
      })

  private def whenAnyKeyedEvents[E <: Event, A](request: EventRequest[E], collect: PartialFunction[AnyKeyedEvent, A]): Future[EventSeq[Iterator, A]] =
    whenAnyKeyedEvents2(request.after, now + (request.timeout min configuration.timeoutLimit), collect, request.limit)

  private def whenAnyKeyedEvents2[A](after: EventId, until: Instant, collect: PartialFunction[AnyKeyedEvent, A], limit: Int): Future[EventSeq[Iterator, A]] =
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

  private def collectEventsSince[A](after: EventId, collect: PartialFunction[AnyKeyedEvent, A], limit: Int): EventSeq[Iterator, A] = {
    require(limit >= 0, "limit must be >= 0")
    val eventSnapshotOption = keyedEventQueue.after(after)
    var lastEventId = after
    eventSnapshotOption match {
      case Some(eventSnapshots) ⇒
        val eventIterator =
          eventSnapshots
            .map { o ⇒ lastEventId = o.eventId; o }
            .collect {
              case snapshot if collect.isDefinedAt(snapshot.value) ⇒
                Snapshot(snapshot.eventId, collect(snapshot.value))
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
  : Iterator[Snapshot[KeyedEvent[E]]]
  =
    keyedEventQueue.reverseEvents(after = request.after)
      .collect {
        case snapshot if request.eventClass isAssignableFrom snapshot.value.event.getClass ⇒
          snapshot.asInstanceOf[Snapshot[KeyedEvent[E]]]
      }
      .filter(snapshot ⇒ predicate(snapshot.value))
      .take(request.limit)

  final def reverseForKey[E <: Event](
    request: ReverseEventRequest[E],
    key: E#Key,
    predicate: E ⇒ Boolean = (_: E) ⇒ true)
  : Iterator[Snapshot[E]]
  =
    keyedEventQueue.reverseEvents(after = request.after)
      .collect {
        case snapshot if request.eventClass isAssignableFrom snapshot.value.event.getClass ⇒
          snapshot.asInstanceOf[Snapshot[KeyedEvent[E]]]
      }
      .collect {
        case Snapshot(eventId, KeyedEvent(`key`, event)) if predicate(event) ⇒
          Snapshot(eventId, event)
      }
      .take(request.limit)

  def wrapInSnapshot[E](future: ⇒ Future[EventSeq[Iterator, E]]): Future[Snapshot[EventSeq[Seq, E]]] = {
    val eventId = eventIdGenerator.next()
    for (eventSeq ← future) yield
      eventSeq match {
        case EventSeq.NonEmpty(iterator) ⇒
          val events = iterator.toVector
          Snapshot(max(eventId, events.last.eventId), EventSeq.NonEmpty(events))
        case o: EventSeq.Empty ⇒
          eventIdGenerator.newSnapshot(o)
        case EventSeq.Torn ⇒
          eventIdGenerator.newSnapshot(EventSeq.Torn)
      }
  }
}

object EventCollector {

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
