package js7.data.event

import js7.base.time.{Timestamp, WallClock}

/** Time context for use as a `Ctx` context for EventCalc and EventColl. */
trait TimeCtx:
  /** Should be immutable. */
  def now: Timestamp

  // TODO Don't use this, use `now`
  final def clock: WallClock =
    WallClock.fixed(now)


object TimeCtx:

  def apply(now: => Timestamp): TimeCtx =
    lazy val now_ = now
    new TimeCtx:
      def now = now_
      override def toString = s"TimeCtx($now)"
