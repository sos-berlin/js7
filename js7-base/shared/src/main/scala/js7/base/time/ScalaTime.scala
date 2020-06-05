package js7.base.time

import cats.Show
import js7.base.convert.As
import js7.base.utils.Ascii.isAsciiDigit
import scala.annotation.tailrec
import scala.concurrent.blocking
import scala.concurrent.duration._
import scala.math.abs
import scala.util.Random

object ScalaTime
{
  private val MaxDuration = Duration(Long.MaxValue, NANOSECONDS)
  private val MinDuration = Duration(Long.MinValue + 1, NANOSECONDS)

  @volatile private var extraSleepCount = 0L

  implicit final class DurationRichInt(private val delegate: Int) extends AnyVal {
    /** Duration, counted in nanoseconds. */
    def ns = Duration(delegate, NANOSECONDS)

    /** Duration, counted in microseconds. */
    def µs = Duration(delegate, MICROSECONDS)

    /**
     * Duration, counted in milliseconds.
     */
    def ms = Duration(delegate, MILLISECONDS)

    /**
     * Duration, counted in seconds.
     */
    def s = Duration(delegate, SECONDS)

    /**
     * Duration, counted in hours.
     */
    def h = Duration(delegate, HOURS)
  }

  implicit final class DurationRichLong(private val delegate: Long) extends AnyVal {
    /** Duration, counted in nanoseconds. */
    def ns = Duration(delegate, NANOSECONDS)

    /** Duration, counted in microseconds. */
    def µs = Duration(delegate, MICROSECONDS)

    /** Duration, counted in milliseconds. */
    def ms = Duration(delegate, MILLISECONDS)

    /** Duration, counted in seconds. */
    def s = Duration(delegate, SECONDS)

    /**
     * Duration, counted in hours.
     */
    def h = Duration(delegate, HOURS)
  }

  implicit final class DurationRichBigDecimal(private val delegate: BigDecimal) extends AnyVal {
    def s: Duration = bigDecimalToDuration(delegate)
  }

  private[time] def bigDecimalToDuration(o: BigDecimal): FiniteDuration = {
    try Duration((o * 1000000000).toLongExact, NANOSECONDS)
    catch { case t: ArithmeticException =>
      throw new ArithmeticException(s"Invalid duration (${t.getMessage}): $o")
    }
  }

  implicit val StringAsDuration: As[String, FiniteDuration] = As(parseDuration)

  implicit val StringAsOptionDuration: As[String, Option[FiniteDuration]] = {
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
  def parseDuration(string: String): FiniteDuration =
    if (string.isEmpty || !isAsciiDigit(string.head))
      throw new IllegalArgumentException(s"Invalid duration: $string")
    else if (string endsWith "ms")
      parse(string.dropRight(2), 1000000)
    else if (string endsWith "µs")
      parse(string.dropRight(2), 1000)
    else if (isAsciiDigit(string.last))
      parse(string, 1000000000)
    else if (string endsWith "s")
      parse(string.dropRight(1), 1000000000)
    else
      throw new IllegalArgumentException(s"Invalid duration: $string")

  private def parse(string: String, factor: Int): FiniteDuration =
    (BigDecimal(string) * factor).toLongExact.nanoseconds.toCoarsest

  def randomDuration(maximum: Duration): Duration = Duration(maximum.toNanos * Random.nextFloat, NANOSECONDS)

  implicit final class RichDuration(private val delegate: Duration) extends AnyVal with Ordered[RichDuration]
  {
    def unary_- = Duration.Zero - delegate
    def min(o: Duration): Duration = if (this <= o) delegate else o
    def max(o: Duration): Duration = if (this > o) delegate else o
    override def toString = pretty  // For ScalaTest

    def msPretty: String = {
      val nanos = delegate.toNanos
      if (nanos == 0) "0ms"
      else {
        val a = abs(nanos)
        if (a >= 1000000000)
          pretty
        else if (a >= 10000000)
          formatNumber(nanos / 1000000.0, 10, "ms")
        else if (a >= 1000000)
          formatNumber(nanos / 1000000.0, 100, "ms")
        else if (a >= 100000)
          formatNumber(nanos / 1000000.0, 1000, "ms")
        else if (a >= 1000)
          formatNumber(nanos / 1000000.0, 1000, "ms")
        else
          pretty
      }
    }

    def pretty: String =
      delegate match {
        case Duration.Zero => "0s"
        case o: FiniteDuration =>
          if (abs(o.toSeconds) < 3*60) smallPretty
          else bigPretty
        case Duration.Inf => "infinite"
        case Duration.Undefined => "undefined"
        case o => o.toString
      }

    private def smallPretty = {
      val nanos = delegate.toNanos
      val a = abs(nanos)
      if (a >= 100_000_000_000L)
        formatNumber(nanos / 1000_000_000.0, 1, "s")
      else if (a >= 10_000_000_000L)
        formatNumber(nanos / 1000_000_000.0, 10, "s")
      else if (a >= 1000_000_000)
        formatNumber(nanos / 1000_000_000.0, 100, "s")
      else if (a >= 100_000_000)
        formatNumber(nanos / 1000_000_000.0, 1000, "s")
      else if (a >= 10_000_000)
        formatNumber(nanos / 1000_000.0, 10, "ms")
      else if (a >= 1000_000)
        formatNumber(nanos / 1000_000.0, 100, "ms")
      else if (a >= 100_000)
        formatNumber(nanos / 1000_000.0, 1000, "ms")
      else if (a >= 10_000)
        formatNumber(nanos / 1000.0, 10, "µs")
      else if (a >= 1000)
        formatNumber(nanos / 1000.0, 100, "µs")
      else if (a >= 100)
        formatNumber(nanos / 1000.0, 1000, "µs")
      else
        s"${nanos}ns"
    }

    private def bigPretty = {
      val seconds = delegate.toSeconds
      val absSeconds = abs(seconds)
      if (absSeconds >= 15*24*60*60)
        s"${seconds / (7*24*60*60)}weeks"
      else if (absSeconds >= 3*24*60*60)
        s"${seconds / (24*60*60)}days"
      else if (absSeconds >= 3*60*60)
        s"${seconds / (60*60)}h"
      else
        s"${seconds / 60}min"
    }

    def compare(o: RichDuration) = delegate compareTo o.delegate
  }

  implicit final class RichFiniteDuration(private val underlying: FiniteDuration) extends AnyVal
  {
    def *(o: BigDecimal): FiniteDuration = bigDecimalToDuration(toBigDecimalSeconds * o)
    def /(o: BigDecimal): FiniteDuration = bigDecimalToDuration(toBigDecimalSeconds / o)

    def withMillis(milliseconds: Int) =
      if (underlying >= Duration.Zero)
        Duration(underlying.toSeconds, SECONDS) + milliseconds.ms
      else
        Duration(underlying.toSeconds, SECONDS) - milliseconds.ms

    def roundUpToNext(granularity: FiniteDuration): FiniteDuration = {
      val nanos = underlying.toNanos
      val sgn = if (nanos >= 0) 1 else -1
      val gran = granularity.toNanos
      Duration((nanos + (gran - 1) * sgn) / gran * gran, NANOSECONDS).toCoarsest
    }

    def toBigDecimalSeconds = underlying.unit match {
      case NANOSECONDS  => BigDecimal(underlying.length, 9)
      case MICROSECONDS => BigDecimal(underlying.length, 6)
      case MILLISECONDS => BigDecimal(underlying.length, 3)
      case SECONDS      => BigDecimal(underlying.length, 0)
      case MINUTES      => BigDecimal(underlying.length) * 60
      case HOURS        => BigDecimal(underlying.length) * (60 * 60)
      case DAYS         => BigDecimal(underlying.length) * (60 * 60 * 24)
    }

    def toDecimalString: String =
      underlying.toBigDecimalSeconds.bigDecimal.toPlainString
  }

  implicit final class RichFiniteDurationCompanion(private val underlying: FiniteDuration.type) extends AnyVal
  {
    def MaxValue: FiniteDuration = MaxDuration
    def MinValue: FiniteDuration = MinDuration
  }

  implicit final class RichDeadline(private val underlying: Deadline) extends AnyVal
  {
    /** Different to isOverdue, this returns true even if timeLeft == 0. */
    def hasElapsed: Boolean =
      underlying.time.toNanos <= System.nanoTime

    def elapsedOrZero: FiniteDuration =
      elapsed max Duration.Zero

    def elapsed: FiniteDuration =
      (System.nanoTime - underlying.time.toNanos).nanoseconds

    def timeLeftOrZero: FiniteDuration =
      underlying.timeLeft max Duration.Zero

    //def roundTo(duration: FiniteDuration): Deadline =
    //  (underlying + duration / 2) roundDownTo duration
    //
    //def roundDownTo(duration: FiniteDuration): Deadline = {
    //  val durationNanos = duration.toNanos
    //  Deadline((underlying.time.toNanos / durationNanos * durationNanos).nanoseconds)
    //}
  }

  def sleep(d: Duration): Unit = {
    val nanos = d.toNanos
    if (nanos > 0) {
      sleepUntil2(nanos, System.nanoTime + nanos)
    }
  }

  def sleepUntil(until: Deadline): Unit = {
    val nanos = until.timeLeft.toNanos
    if (nanos > 0) {
      sleepUntil2(nanos, until.time.toNanos)
    }
  }

  private def sleepUntil2(nanos: Long, until: Long): Unit =
    blocking {
      nanoSleep(nanos)
      extraSleepUntil(until)
    }

  @tailrec
  private def extraSleepUntil(until: Long): Unit = {
    val remainingNanos = until - System.nanoTime
    if (remainingNanos > 0) {
      extraSleepCount += 1
      nanoSleep(remainingNanos)
      extraSleepUntil(until)
    }
  }

  private def nanoSleep(nanos: Long) = {
    val m = 1000*1000
    Thread.sleep(nanos / m, (nanos % m).toInt)
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
      if (result(truncated - 1) == '.') truncated -= 1
      result.delete(truncated, result.length)
    }
    result ++= suffix
    result.toString
  }

  implicit val ScalaDurationShow: Show[Duration] = _.pretty
  implicit val FiniteDurationShow: Show[FiniteDuration] = _.pretty
}
