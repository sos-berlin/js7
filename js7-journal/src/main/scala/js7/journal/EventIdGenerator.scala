package js7.journal

import js7.base.time.{SystemWallClock, Timestamp, WallClock}
import js7.base.utils.Atomic
import js7.data.event.{EventId, Stamped}
import scala.annotation.tailrec
import scala.collection.AbstractIterator

/**
  * @author Joacim Zschimmer
  */
final class EventIdGenerator(clock: WallClock)
extends AbstractIterator[EventId]:
  def this() = this(WallClock)

  private val lastResult = Atomic(EventId.BeforeFirst)

  def lastUsedEventId: EventId = lastResult.get

  def updateLastEventId(newEventId: EventId): Unit =
    while true do
      val last = lastResult.get
      if newEventId < last then return
      if lastResult.compareAndSet(last, newEventId) then return

  def hasNext = true

  @tailrec
  def next(): EventId =
    val nowId = clock.epochMilli() * EventId.IdsPerMillisecond
    val last = lastResult.get
    val nextId = if last < nowId then nowId else last + 1
    if lastResult.compareAndSet(last, nextId) then
      nextId
    else
      next()

  def stamp[A](a: A, timestampMillis: Option[Long] = None): Stamped[A] =
    stampWith(a, next(), timestampMillis orElse defaultTimestampMilli())

  /** Some() iff clock is a manipulated clock, for testing or checking. */
  private val defaultTimestampMilli: () => Option[Long] =
    clock match
      case _: SystemWallClock =>
        () => None
      case _ =>
        () => Some(clock.epochMilli())

  private def stampWith[A](a: A, eventId: EventId, timestampMillis: Option[Long]): Stamped[A] =
    val ts = timestampMillis getOrElse EventId.toEpochMilli(eventId)
    new Stamped(eventId, ts, a)


object EventIdGenerator:

  def withFixedClock(epochMilli: Long): EventIdGenerator =
    EventIdGenerator(WallClock.fixed(Timestamp.ofEpochMilli(epochMilli)))
