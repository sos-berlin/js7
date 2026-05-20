package js7.base.time

import js7.base.time.ScalaTime.*
import js7.base.time.SpeedLimiter.{Record, TooFast}
import scala.concurrent.duration.FiniteDuration

/**
  * A time-based limiter that limits weight per period.
  *
  * Might be seen as a _speed limiter_, if weight is added distance.
  *
  * Could also seen as an _overheating protector_,
  * if  weight is seen as heating.
  * While time passes without heating, the thing cools down.
  *
  * Limits are expressed as weights per period.
  *
  * Immutable.
  */
final class StandardSpeedLimiter private(
  speedLimits: Seq[Speed],
  histogram: TimeHistogram,
  startTime: FiniteDuration)
extends SpeedLimiter:

  protected type Self = StandardSpeedLimiter

  def setTime(time: FiniteDuration): StandardSpeedLimiter =
    new StandardSpeedLimiter(
      speedLimits,
      histogram.setTime(time - startTime),
      startTime)

  /** Try to record a weight while checking the limit.
    *
    * Or "Try to accelerate but check the speed limit"
    * Or "Try to heat up but the temperature limit"
    *
    * @return `Left[TooFast]` if the speed limit would be exceeded.
    *         The weight may be added after the retured `delay` has passed.
    *         `Right[StandardSpeedLimiter]` if acceleration was possible.
    */
  def tryRecord(record: Record): Either[TooFast, StandardSpeedLimiter] =
    val updated = this.record(record)
    updated.tooFast.toLeft(updated)

  /** Like [[tryRecord]] but doesn't check speed limit. */
  def record(record: Record): StandardSpeedLimiter =
    if speedLimits.isEmpty then
      this
    else
      new StandardSpeedLimiter(
        speedLimits,
        histogram = histogram.add(record.time - startTime, record.weight),
        startTime = startTime)

  /** Return the remaining time until the speed has lowered below the limits.
    *
    * @return `Some[TooFast]` if the speed is above a limit.
    *         `None` if the speed is not above a limit .
    */
  private def tooFast: Option[TooFast] =
    speedLimits.iterator.zipWithIndex.map:
      case (speedLimit, i) => speedLimit -> histogram.recordedSpeed(i)
    .collect:
      case (speedLimit, recordedSpeed) if recordedSpeed.speed.weight > speedLimit.weight =>
        TooFast(
          speedLimit,
          delay = recordedSpeed.end - time)
          //delay = speedLimit.period - (time.toNanos % speedLimit.period.toNanos).ns.toCoarsest)
    .maxByOption(_.delay)

  private def time: FiniteDuration =
    histogram.time - startTime

  override def toString = s"StandardSpeedLimiter($histogram)"


object StandardSpeedLimiter:

  def apply(
    speeds: Seq[Speed],
    startTime: FiniteDuration = ZeroDuration,
    fractions: Int = 10)
  : StandardSpeedLimiter =
    new StandardSpeedLimiter(
      speeds,
      TimeHistogram(speeds.map(_.period), fractions),
      startTime)
