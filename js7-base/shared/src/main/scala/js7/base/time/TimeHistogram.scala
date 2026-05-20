package js7.base.time

import java.lang.System.arraycopy
import js7.base.time.ScalaTime.*
import js7.base.time.TimeHistogram.*
import scala.concurrent.duration.*

/**
  * A histogram with increasing periods, which counts the number of events in each period..
  * Each period starts at it's multiple end ends at the time set via setTime.
  *
  * For example, a minute period starts at each full minute. Three seconds later,
  * this period spans only these three seconds.
  *
  * Immutable.
  *
  * @param time the last recorded time
  * @param for each period, the recorded weight for each fraction of the period
  * @param Number of fractions of each period
  */
final class TimeHistogram private(
  val time: FiniteDuration,
  entries: IArray[Entry],
  fractions: Int):

  /** Set the current time as a FiniteDuration since some start time.
    * Each time must be greater or equal to the previous time.
    */
  def setTime(newTime: FiniteDuration): TimeHistogram =
    add(newTime, 0)

  def add(weight: Double): TimeHistogram =
    add(entries.head.start.ns, weight)

  def add(time: FiniteDuration, weight: Double): TimeHistogram =
    val timeNanos = time.toNanos
    if timeNanos < entries.head.start then throw new IllegalArgumentException(
      s"Time must be greater than current time: $time < ${entries.head.start.ns}")
    new TimeHistogram(time, entries.map(_.add(timeNanos, weight)), fractions)

  /** @throws IllegalArgumentException if `period` is not known.
    */
  def weight(period: FiniteDuration): Double =
    weight(indexOf(period))

  def weight(periodIndex: Int): Double =
    entries(periodIndex).weight

  /** @throws IllegalArgumentException if `period` is not known.
    */
  def speed(period: FiniteDuration): Speed =
    speed(indexOf(period))

  def speed(periodIndex: Int): Speed =
    entries(periodIndex).speed

  /** @throws IllegalArgumentException if `period` is not known.
    */
  def recordedSpeed(period: FiniteDuration): RecordedSpeed =
    recordedSpeed(indexOf(period))

  def recordedSpeed(periodIndex: Int): RecordedSpeed =
    entries(periodIndex).recordedSpeed

  /** @throws IllegalArgumentException if `period` is not known.
    */
  private def indexOf(period: FiniteDuration): Int =
    val i = entries.indexWhere(_.period == period.toNanos)
    if i < 0 then throw new IllegalArgumentException(s"Unknown period: $period")
    i

  def periods: Seq[FiniteDuration] =
    entries.map(_.period.ns)

  override def toString =
    s"TimeHistogram($time, fractions=$fractions, ${entries.mkString(" ")})"


object TimeHistogram:

  /**
    * @param periods For example, `Seq(1.s, 1.minute, 1.h)` provides the weights for the last
    *                second, minute and hour, i.e. the period that ends at the current time.
    *
    * @param fractions The number of fractions per period.
    *                  Higher numbers yield more precision, but also more memory.
    */
  def apply(periods: Seq[FiniteDuration], fractions: Int = 10): TimeHistogram =
    new TimeHistogram(
      ZeroDuration,
      entries =
        periods.map: period =>
          Entry(start = 0, period.toNanos, IArray.fill(fractions)(0))
       .toArray.asInstanceOf[IArray[Entry]],
      fractions = fractions)

  /**
    * Recorded weights (on per fraction) in the last period starting at start.
    * @param start  the start at which the period starts, in nanoseconds
    * @param period speed limit period, in nanoseconds
    * @param weights the recorded weights for each fraction of the period
    *                `weight(0)` is the latest recorded weight
    */
  private final class Entry(val start: Long, val period: Long, weights: IArray[Double]):
    private val periodFraction = period / weights.length

    def setTime(newTime: Long): Entry =
      add(newTime, 0)

    def add(weight: Double): Entry =
      add(start, weight)

    def add(newTime: Long, weight: Double): Entry =
      val shift = (newTime - start) / periodFraction
      if shift <= 0 && weight == 0 then
        this
      else
        val newCounters = new Array[Double](weights.length)
        val shft = (shift min weights.length).toInt
        arraycopy(weights, 0, newCounters, shft, weights.length - shft)
        newCounters(0) += weight
        Entry(
          start = start + periodFraction * shift,
          period,
          weights = newCounters.asInstanceOf[IArray[Double]])
      end if

    def recordedSpeed: RecordedSpeed =
      RecordedSpeed(start.ns, speed)

    def speed: Speed =
      Speed(weight, period.ns)

    def weight: Double =
      weights.sum

    override def toString =
      s"Entry(${period.ns.show}: start=${start.ns.show}, weights=${
        weights.map(_.toString.stripSuffix(".0")).mkString(" ")})"


  final case class RecordedSpeed(start: FiniteDuration, speed: Speed):
    def end: FiniteDuration =
      start + speed.period
