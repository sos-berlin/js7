package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.event.{EventId, EventSeq, Stamped, TearableEventSeq}
import java.util.concurrent.atomic.AtomicLong
import javax.inject.{Inject, Singleton}
import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class EventIdGenerator @Inject()(clock: EventIdClock = EventIdClock.Default) extends Iterator[EventId] {

  private val lastResult = new AtomicLong(EventId.BeforeFirst)

  def lastUsedEventId: EventId = lastResult.get

  def hasNext = true

  @tailrec
  def next(): EventId = {
    val nowId = clock.currentTimeMillis * EventId.IdsPerMillisecond
    val last = lastResult.get
    val nextId = if (last < nowId) nowId else last + 1
    if (lastResult.compareAndSet(last, nextId))
      nextId
    else
      next()
  }

  /**
    * Wraps `EventCollector`'s `TearableEventSeq` (Iterator-based) into a Stamped with own `EventId`.
    */
  def stampTearableEventSeq[E](future: ⇒ Future[TearableEventSeq[Iterator, E]])(implicit ec: ExecutionContext): Future[Stamped[TearableEventSeq[Seq, E]]] = {
    for (eventSeq ← future) yield
      eventSeq match {
        case eventSeq: EventSeq[Iterator,E] ⇒
          val Stamped(eventId, timestamp, ()) = stampWithLast(())
          stampEventSeqWith(eventSeq, eventId, timestamp)
        case o: TearableEventSeq.Torn ⇒
          stampWithLast(o)
      }
  }

  /**
    * Wraps an `EventSeq` (Iterator-based) into a Stamped with own `EventId`.
    */
  private def stampEventSeq[E](future: ⇒ Future[EventSeq[Iterator, E]])(implicit ec: ExecutionContext): Future[Stamped[EventSeq[Seq, E]]] = {
    val Stamped(eventId, timestamp, ()) = stampWithLast(())
    for (eventSeq ← future) yield stampEventSeqWith(eventSeq, eventId, timestamp)
  }

  private def stampEventSeqWith[E](eventSeq: EventSeq[Iterator, E], eventId: EventId, timestamp: Timestamp) =
    eventSeq match {
      case EventSeq.NonEmpty(eventsIterator) ⇒
        Stamped(math.max(eventId, lastUsedEventId), timestamp, EventSeq.NonEmpty(eventsIterator.toVector))
      case o: EventSeq.Empty ⇒
        Stamped(eventId, timestamp, o)
    }

  def stamp[A](a: A, timestamp: Option[Timestamp] = None): Stamped[A] =
    stampWith(a, next(), timestamp)

  def stampWithLast[A](a: A, timestamp: Option[Timestamp] = None): Stamped[A] =
    stampWith(a, lastUsedEventId, timestamp)

  private def stampWith[A](a: A, eventId: EventId, timestamp: Option[Timestamp] = None): Stamped[A] =
    Stamped(eventId, timestamp getOrElse EventId.toTimestamp(eventId), a)
}
