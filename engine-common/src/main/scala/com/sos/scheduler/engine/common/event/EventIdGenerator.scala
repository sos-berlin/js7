package com.sos.scheduler.engine.common.event

import com.sos.scheduler.engine.data.event.{EventId, EventSeq, Snapshot}
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
    * Wraps `EventCollector`'s `EventSeq` (Iterator-based) into a Snapshot with own `EventId`.
    */
  def wrapInSnapshot[E](future: ⇒ Future[EventSeq[Iterator, E]])(implicit ec: ExecutionContext): Future[Snapshot[EventSeq[Seq, E]]] = {
    val eventId = next()
    for (eventSeq ← future) yield
      eventSeq match {
        case EventSeq.NonEmpty(eventsIterator) ⇒
          Snapshot(math.max(eventId, lastUsedEventId), EventSeq.NonEmpty(eventsIterator.toVector))
        case o: EventSeq.Empty ⇒
          newSnapshot(o)
        case EventSeq.Torn ⇒
          newSnapshot(EventSeq.Torn)
      }
  }

  def newSnapshot[A](a: A) = Snapshot(next(), a)
}
