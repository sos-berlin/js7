package js7.base.time

import cats.Show
import java.time._
import js7.base.convert.As
import js7.base.utils.Ascii.isAsciiDigit
import org.jetbrains.annotations.TestOnly
import scala.math.abs

object JavaTime
{
  val MaxDuration = Duration.ofSeconds(Long.MaxValue, 999999999)
  val Iso8601DurationRegex = "[0-9.A-Za-z]+".r

  @TestOnly @volatile var extraSleepCount = 0L

  implicit final class DurationRichInt(private val delegate: Int) extends AnyVal {
    /**
     * Duration, counted in microseconds.
     */
    def µs = Duration ofNanos 1000L * delegate

    /**
     * Duration, counted in milliseconds.
     */
    def ms = Duration ofMillis delegate

    /**
     * Duration, counted in seconds.
     */
    def s = Duration ofSeconds delegate

    // Conflicts with scala.runtime.RichInt.min
    ///**
    // * Duration, counted in minutes.
    // */
    //def min = Duration ofMinutes delegate

    /**
     * Duration, counted in hours.
     */
    def h = Duration ofHours delegate

    def *(o: Duration) =  o multipliedBy delegate
  }

  implicit final class DurationRichLong(private val delegate: Long) extends AnyVal {
    /**
     * Duration, counted in microseconds.
     */
    def µs = Duration.ofSeconds(delegate / 1000000, delegate % 1000000 * 1000)

    /**
     * Duration, counted in milliseconds.
     */
    def ms = Duration ofMillis delegate

    /**
     * Duration, counted in seconds.
     */
    def s = Duration ofSeconds delegate

    // Conflicts with scala.runtime.RichLong.min
    ///**
    // * Duration, counted in minutes.
    // */
    //def min = Duration ofMinutes delegate

    /**
     * Duration, counted in hours.
     */
    def h = Duration ofHours delegate

    def *(o: Duration) = o multipliedBy delegate
  }

  implicit final class DurationRichBigDecimal(private val delegate: BigDecimal) extends AnyVal {
    def s: Duration = bigDecimalToDuration(delegate)
  }

  def bigDecimalToDuration(o: BigDecimal): Duration = {
    val (seconds, fraction) = o /% 1
    try Duration.ofSeconds(seconds.toLongExact, (fraction * 1000*1000*1000).toIntExact)
    catch { case t: ArithmeticException =>
      throw new ArithmeticException(s"Value '$o' is not a Duration: ${Option(t.getMessage) getOrElse t}")
    }
  }

  implicit val StringAsDuration: As[String, Duration] = As(parseDuration)
  implicit val StringAsOptionDuration: As[String, Option[Duration]] = {
    case "never" | "eternal" | "" => None
    case string => Some(parseDuration(string))
  }

  /**
    * Parses a duration according to ISO-8601 with optional first letters PT.
    * <p>The string may like
    * <ul>
    *   <li>ISO-8601, like "PT3M30S" for 3 minutes and 30 seconds,
    *   <li>"123s" or "123.456s" for a decimal number of seconds,
    *   <li>"123" or "123.456" for a decimal number denoting seconds
    * </ul>
    */
  def parseDuration(string: String): Duration =
    if (string.nonEmpty && isAsciiDigit(string.head))
      if (isAsciiDigit(string.last))
        Duration parse s"PT${string}S"
      else
      if (string endsWith "ms")
        Duration ofMillis string.dropRight(2).toLong
      else
      if (string endsWith "µs")
        string.dropRight(2).toLong.µs
      else
        Duration parse s"PT$string"
    else
      Duration parse string

  implicit final class RichDuration(private val delegate: Duration) extends AnyVal with Ordered[RichDuration] {
    def unary_- = Duration.ZERO minus delegate
    def +(o: Duration): Duration = delegate plus o
    def -(o: Duration): Duration = delegate minus o
    def *(o: Long): Duration = delegate multipliedBy o
    def /(o: Long): Duration = delegate dividedBy o
    def *(o: BigDecimal): Duration = bigDecimalToDuration(delegate.toBigDecimal * o)
    def /(o: BigDecimal): Duration = bigDecimalToDuration(delegate.toBigDecimal / o)
    def min(o: Duration): Duration = if (this <= o) delegate else o
    def max(o: Duration): Duration = if (this > o) delegate else o
    def toBigDecimal = BigDecimal(delegate.getSeconds) + BigDecimal(delegate.getNano) / (1000*1000*1000)
    override def toString = pretty  // For ScalaTest

    def pretty: String =
      if (delegate == Duration.ZERO)
        "0s"
      else if (abs(delegate.getSeconds) < 3*60)
        smallPretty
      else
        bigPretty

    private def smallPretty = {
      val nanos = delegate.toNanos
      val a = abs(nanos)
      if (a >= 100000000)
        formatNumber(nanos / 1000000000.0, 1000, "s")
      else if (a >= 10000000)
        formatNumber(nanos / 1000000.0, 10, "ms")
      else if (a >= 1000000)
        formatNumber(nanos / 1000000.0, 100, "ms")
      else if (a >= 100000)
        formatNumber(nanos / 1000000.0, 1000, "ms")
      else if (a >= 10000)
        formatNumber(nanos / 1000.0, 10, "µs")
      else if (a >= 1000)
        formatNumber(nanos / 1000.0, 100, "µs")
      else if (a >= 100)
        formatNumber(nanos / 1000.0, 1000, "µs")
      else
        s"${nanos}ns"
    }

    private def bigPretty = {
      val seconds = delegate.getSeconds
      val absSeconds = abs(seconds)
      if (absSeconds >= 3*366*24*60*60)
        s"${seconds / (366*24*60*60)}~years"
      else if (absSeconds >= 366*24*60*60)
        s"${seconds / (30*24*60*60)}~months"
      else if (absSeconds >= 3*24*60*60)
        s"${seconds / (24*60*60)}days"
      else if (absSeconds >= 3*60*60)
        s"${seconds / (60*60)}h"
      else
        s"${seconds / 60}min"
    }

    def compare(o: RichDuration) = delegate compareTo o.delegate
  }

  implicit final class RichInstant(private val delegate: Instant) extends AnyVal with Ordered[RichInstant] {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
    def -(o: Instant) = Duration.between(o, delegate)
    def min(o: Instant): Instant = if (this <= o) delegate else o
    def max(o: Instant): Instant = if (this > o) delegate else o
    def compare(o: RichInstant) = delegate compareTo o.delegate

    def roundTo(duration: Duration): Instant = this + duration / 2 roundDownTo duration

    def roundDownTo(duration: Duration): Instant = {
      val durationMillis = duration.toMillis
      Instant.ofEpochMilli(delegate.toEpochMilli / durationMillis * durationMillis)
    }

    override def toString = delegate.toString  // For ScalaTest
  }

  implicit final class RichLocalTime(private val delegate: LocalTime) extends AnyVal with Ordered[RichLocalTime] {
    def compare(o: RichLocalTime) = delegate compareTo o.delegate
  }

  implicit final class RichLocalDateTime(private val delegate: LocalDateTime) extends AnyVal with Ordered[RichLocalDateTime] {
    def compare(o: RichLocalDateTime) = delegate compareTo o.delegate
    def toInstant(zone: ZoneId) = delegate.toInstant(zone.getRules.getOffset(delegate))
  }

  private def formatNumber(number: Double, divisor: Int, suffix: String) = {
    val result = new StringBuilder(11 + suffix.length)
    if (number < 0) result += '-'
    val a = abs(number)
    val integral = a.toInt
    result.append(integral)
    val b = ((a - integral) * divisor + 0.5).toInt
    if (b > 0) {
      val p = result.length
      result.append(divisor + b)
      result(p) = '.'
      var truncated = result.length
      while (result(truncated - 1) == '0') truncated -= 1
      result.delete(truncated, result.length)
    }
    result ++= suffix
    result.toString
  }

  def dateToInstant(date: java.util.Date): Instant = Instant.ofEpochMilli(date.getTime)

  implicit val JavaUtilDateShow = Show[java.util.Date](o =>
    JavaTimestamp(o).show)
}
