package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.data.event.{EventId, EventSeq, Stamped, TearableEventSeq}
import java.lang.System._
import java.util.concurrent.atomic.AtomicLong
import javax.inject.{Inject, Singleton}
import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class EventIdGenerator @Inject extends Iterator[EventId] {

  private val lastResult = new AtomicLong(EventId.BeforeFirst)

  def lastUsedEventId: EventId = lastResult.get

  def hasNext = true

  @tailrec
  def next(): EventId = {
    val nowId = currentTimeMillis * EventId.IdsPerMillisecond
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
    val eventId = next()
    for (eventSeq ← future) yield
      eventSeq match {
        case eventSeq: EventSeq[Iterator,E] ⇒
          stampEventSeqWith(eventSeq, eventId)
        case EventSeq.Torn ⇒
          stamp(EventSeq.Torn)
      }
  }

  /**
    * Wraps an `EventSeq` (Iterator-based) into a Stamped with own `EventId`.
    */
  def stampEventSeq[E](future: ⇒ Future[EventSeq[Iterator, E]])(implicit ec: ExecutionContext): Future[Stamped[EventSeq[Seq, E]]] = {
    val eventId = next()
    for (eventSeq ← future) yield stampEventSeqWith(eventSeq, eventId)
  }

  private def stampEventSeqWith[E](eventSeq: EventSeq[Iterator, E], eventId: EventId) =
    eventSeq match {
      case EventSeq.NonEmpty(eventsIterator) ⇒
        Stamped(math.max(eventId, lastUsedEventId), EventSeq.NonEmpty(eventsIterator.toVector))
      case o: EventSeq.Empty ⇒
        stamp(o)
    }

  def stamp[A](a: A) = Stamped(next(), a)
}
