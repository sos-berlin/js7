package js7.journal

import js7.base.time.{TestWallClock, Timestamp, WallClock}

/**
  * @author Joacim Zschimmer
  */
final class EventIdClock(val clock: WallClock):
  def this() = this(WallClock)

  /** Current time in milliseconds since 1970-01-01 UTC, like Java currentTimeMillis. */
  def currentTimeMillis: Long =
    clock.epochMilli()

object EventIdClock:
  val Default = EventIdClock(WallClock)

  def apply(clock: WallClock): EventIdClock =
    new EventIdClock(clock)

  def fixed(epochMilli: Long) =
    new EventIdClock(TestWallClock(Timestamp.ofEpochMilli(epochMilli)))
