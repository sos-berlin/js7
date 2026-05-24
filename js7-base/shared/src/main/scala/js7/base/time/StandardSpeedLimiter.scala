package js7.base.time

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
  private val speedLimits: Seq[Speed],
  private val histogram: TimeHistogram,
  unit: SpeedUnit)
extends SpeedLimiter:

  protected type Self = StandardSpeedLimiter

  def setTime(time: FiniteDuration): StandardSpeedLimiter =
    new StandardSpeedLimiter(
      speedLimits,
      histogram.setTime(time),
      unit)

  /** Try to record a weight while checking the limit.
    *
    * Or "Try to accelerate but check the speed limit"
    * Or "Try to heat up but the temperature limit"
    *
    * @return `Left[TooFast]` if the speed limit is exceeded.
    *         The weight may be added after the retured `delay` has passed.
    *         `Right[StandardSpeedLimiter]` if acceleration was possible.
    */
  def tryRecord(record: Record): Either[TooFast, StandardSpeedLimiter] =
    val updated = this.record(record)
    updated.speedLimits.iterator.zipWithIndex.map:
      case pair @ (speedLimit, i) => (pair, updated.histogram.recordedSpeed(i))
    .collect:
      case ((speedLimit, i), recordedSpeed) if recordedSpeed.speed.weight > speedLimit.weight =>
        val end = histogram.recordedSpeed(i).end
        TooFast(speedLimit, delay = end - record.time)
    .maxByOption(_.delay)
    .toLeft(updated)

  /** Like [[tryRecord]] but doesn't check speed limit. */
  def record(record: Record): StandardSpeedLimiter =
    if speedLimits.isEmpty then
      this
    else
      new StandardSpeedLimiter(
        speedLimits,
        histogram = histogram.add(record.time, record.weight),
        unit)

  private def time: FiniteDuration =
    histogram.time

  override def toString =
    s"StandardSpeedLimiter(${speedLimits.mkString("(", ", ", ")")}, $histogram)"


object StandardSpeedLimiter:

  def apply(speeds: Seq[Speed], fractions: Int = 10, unit: SpeedUnit = SpeedUnit.empty)
  : StandardSpeedLimiter =
    new StandardSpeedLimiter(
      speeds,
      TimeHistogram(speeds.map(_.period), fractions, unit),
      unit)
