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
  unit: SpeedUnit,
  private val histogram: TimeHistogram)
extends Throttle:

  protected type Self = StandardThrottle

  def setTime(time: FiniteDuration): StandardThrottle =
    updateHistogram(histogram.setTime(time))

  /** Try to record a weight while checking the limit.
    *
    * Or "Try to accelerate but check the speed limit".<br>
    * Or "Try to heat up but check the temperature limit".
    *
    * @return `Left[TooFast]` if the speed limit is exceeded.
    *         The weight may be added after the retured `delay` has passed.
    *         `Right[StandardThrottle]` if acceleration was possible.
    */
  def tryRecord(record: Record): Either[TooFast, StandardThrottle] =
    val updated = this.record(record)
    updated.checkSpeedLimit(histogram, record)

  private def checkSpeedLimit(unbreachedHistogram: TimeHistogram, record: Record)
  : Either[TooFast, StandardThrottle] =
    throttles.iterator.zipWithIndex.map:
      case pair @ (throttle, i) => (pair, histogram.recordedSpeed(i))
    .collect:
      case ((throttle, i), recordedSpeed) if recordedSpeed.speed.weight > throttle.weight =>
        TooFast(throttle, delay = unbreachedHistogram.recordedSpeed(i).end - record.time)
    .maxByOption(_.delay)
    .toLeft(this)

  def record(record: Record): StandardThrottle =
    if throttles.isEmpty then
      this
    else
      updateHistogram(histogram.add(record.time, record.weight))

  private def time: FiniteDuration =
    histogram.time

  private def updateHistogram(histogram: TimeHistogram): StandardThrottle =
    new StandardThrottle(throttles, unit, histogram)

  override def toString =
    s"StandardThrottle(${throttles.mkString("(", ", ", ")")}, $histogram)"


object StandardThrottle:

  def apply(speeds: Seq[Speed], fractions: Int = 10, unit: SpeedUnit = SpeedUnit.empty)
  : StandardThrottle =
    new StandardThrottle(
      speeds,
      unit,
      TimeHistogram(speeds.map(_.period), fractions, unit))
