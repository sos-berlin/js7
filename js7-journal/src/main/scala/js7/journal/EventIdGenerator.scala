package js7.journal

import java.util.concurrent.atomic.AtomicLong
import js7.base.time.WallClock
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{EventId, Stamped}
import scala.annotation.tailrec
import scala.collection.AbstractIterator

/**
  * @author Joacim Zschimmer
  */
final class EventIdGenerator(eventIdClock: EventIdClock)
extends AbstractIterator[EventId]
{
  def this() = this(EventIdClock.Default)

  private val lastResult = new AtomicLong(EventId.BeforeFirst)

  def lastUsedEventId: EventId = lastResult.get

  def updateLastEventId(newEventId: EventId): Unit =
    while (true) {
      val last = lastResult.get
      if (newEventId < last) return
      if (lastResult.compareAndSet(last, newEventId)) return
    }

  def hasNext = true

  @tailrec
  def next(): EventId = {
    val nowId = eventIdClock.currentTimeMillis * EventId.IdsPerMillisecond
    val last = lastResult.get
    val nextId = if (last < nowId) nowId else last + 1
    if (lastResult.compareAndSet(last, nextId))
      nextId
    else
      next()
  }

  def stamp[A](a: A, timestampMillis: Option[Long] = None): Stamped[A] =
    stampWith(a, next(),
      timestampMillis.orElse(
        !isWallclock ? eventIdClock.clock.epochMilli()))

  private def stampWith[A](a: A, eventId: EventId, timestampMillis: Option[Long]): Stamped[A] = {
    val ts = timestampMillis getOrElse EventId.toEpochMilli(eventId)
    new Stamped(eventId, ts, a)
  }

  @inline private def isWallclock =
    eventIdClock.clock eq WallClock
}
