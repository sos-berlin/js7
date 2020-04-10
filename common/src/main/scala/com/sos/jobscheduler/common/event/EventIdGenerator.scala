package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.event.{EventId, Stamped}
import java.util.concurrent.atomic.AtomicLong
import javax.inject.{Inject, Singleton}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class EventIdGenerator @Inject()(clock: EventIdClock = EventIdClock.Default) extends Iterator[EventId] {

  private val lastResult = new AtomicLong(EventId.BeforeFirst)

  def lastUsedEventId: EventId = lastResult.get

  def updateLastEventId(newEventId: EventId): Unit =
    while (true) {
      val e = lastResult.get
      if (e >= newEventId) return
      if (lastResult.compareAndSet(e, newEventId)) return
    }

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

  def stamp[A](a: A, timestamp: Option[Timestamp] = None): Stamped[A] =
    stampWith(a, next(), timestamp)

  private def stampWith[A](a: A, eventId: EventId, timestamp: Option[Timestamp]): Stamped[A] =
    Stamped(eventId, timestamp getOrElse EventId.toTimestamp(eventId), a)
}
