package js7.base.time

import java.lang.System.arraycopy
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.*

/** A kind of histogram which counts the number of events in time periods.
  * Each period ends at the time set via setTime.
  *
  * For example, `SimpleTimeHistogram(1.s, limit = 3600)` provides the counters for the
  * last 3600 seconds.
  *
  * Requires (8 × limit) bytes of memory.
  */
final class SimpleTimeHistogram(val period: FiniteDuration, limit: Int):

  private val periodNanos = period.toNanos
  private val counters = new Array[Long](limit)
  private var _time = ZeroDuration

  require(limit > 0, "limit must be > 0")

  /** Increment the counter for the current time (as set with setTime). */
  def add(n: Long): Unit =
    counters(0) += n

  // TODO No throws
  /** Set the current time as a FiniteDuration since some start time.
    * Each time must be greater or equal to the previous time.
    */
  def setTime(newTime: FiniteDuration): Unit =
    if newTime < _time then throw new IllegalArgumentException(
      s"Time must be greater than current time: $newTime < $_time")
    val shift = (newTime - _time).toNanos / periodNanos
    if shift > 0 then
      _time += period * shift
      val shft = (shift min limit).toInt
      arraycopy(counters, 0, counters, shft, limit - shft)
      java.util.Arrays.fill(counters, 0, shft, 0)
    end if

  /** Return number of events in the last `n` × `period`. */
  def last(n: Int): Long =
    counters.iterator.take(n).sum

  override def toString = s"SimpleTimeHistogram(${period.pretty}, $limit)"
