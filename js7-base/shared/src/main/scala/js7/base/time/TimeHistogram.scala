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
  * `TimeHistogram(Seq(1.s, 1.minute, 1.h))` provides the counters for the last
  * second, minute and hour, i.e. the period that ends at the current time.
  */
final class TimeHistogram(val periods: Seq[FiniteDuration], fractions: Int = 10):
  // TODO Don't throw

  private val entries = periods.map(p => Entry(p.toNanos, 0, fractions)).toArray

  /** Set the current time as a FiniteDuration since some start time.
    * Each time must be greater or equal to the previous time.
    */
  def setTime(newTime: FiniteDuration): Unit =
    val newTimeNanos = newTime.toNanos
    if newTimeNanos < entries.head.time then throw new IllegalArgumentException(
      s"Time must be greater than current time: $newTime < ${entries.head.time.ns}")
    entries.foreach: entry =>
      entry.setTime(newTimeNanos)

  /** Increment the counter for the current time (as set with setTime). */
  def add(n: Long): Unit =
    entries.foreach:
      _.add(n)

  /** Return number of events in the last `period`. */
  def last(period: FiniteDuration): Long =
    entries.find(_.period == period.toNanos).getOrElse:
      throw new IllegalArgumentException(s"Unknown period: $period")
    .counter


  override def toString = s"TimeHistogram(${periods.map(_.pretty).mkString(" ")})"


object TimeHistogram:

  private final class Entry(val period: Long, var time: Long, fractions: Int):
    private val periodFraction = period / fractions
    private val counters = new Array[Long](fractions)

    def setTime(newTime: Long) =
      val shift = (newTime - time) / periodFraction
      if shift > 0 then
        time += periodFraction * shift
        val shft = (shift min fractions).toInt
        arraycopy(counters, 0, counters, shft, fractions - shft)
        java.util.Arrays.fill(counters, 0, shft, 0)
      end if

    def add(n: Long): Unit =
      counters(0) += n

    def counter: Long =
      counters.sum
