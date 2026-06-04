package js7.base.time

import js7.base.time.Throttle.{Record, TooFast}
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
final class StandardThrottle private(
  private val throttles: Seq[Speed],
  private val histogram: TimeHistogram,
  unit: SpeedUnit)
extends Throttle:

  protected type Self = StandardThrottle

  def setTime(time: FiniteDuration): StandardThrottle =
    new StandardThrottle(
      throttles,
      histogram.setTime(time),
      unit)

  /** Try to record a weight while checking the limit.
    *
    * Or "Try to accelerate but check the speed limit"
    * Or "Try to heat up but the temperature limit"
    *
    * @return `Left[TooFast]` if the speed limit is exceeded.
    *         The weight may be added after the retured `delay` has passed.
    *         `Right[StandardThrottle]` if acceleration was possible.
    */
  def tryRecord(record: Record): Either[TooFast, StandardThrottle] =
    val updated = this.record(record)
    updated.throttles.iterator.zipWithIndex.map:
      case pair @ (throttle, i) => (pair, updated.histogram.recordedSpeed(i))
    .collect:
      case ((throttle, i), recordedSpeed) if recordedSpeed.speed.weight > throttle.weight =>
        val end = histogram.recordedSpeed(i).end
        TooFast(throttle, delay = end - record.time)
    .maxByOption(_.delay)
    .toLeft(updated)

  /** Like [[tryRecord]] but doesn't check speed limit. */
  def record(record: Record): StandardThrottle =
    if throttles.isEmpty then
      this
    else
      new StandardThrottle(
        throttles,
        histogram = histogram.add(record.time, record.weight),
        unit)

  private def time: FiniteDuration =
    histogram.time

  override def toString =
    s"StandardThrottle(${throttles.mkString("(", ", ", ")")}, $histogram)"


object StandardThrottle:

  def apply(speeds: Seq[Speed], fractions: Int = 10, unit: SpeedUnit = SpeedUnit.empty)
  : StandardThrottle =
    new StandardThrottle(
      speeds,
      TimeHistogram(speeds.map(_.period), fractions, unit),
      unit)
