package js7.base.time

import cats.Show
import js7.base.convert.As
import js7.base.utils.Ascii.isAsciiDigit
import scala.annotation.tailrec
import scala.concurrent.blocking
import scala.concurrent.duration._
import scala.math.{abs, floor}
import scala.util.Random

object ScalaTime
{
  /** This is 0 seconds, while Scala's Duration Zero is 0 days. */
  val ZeroDuration = Duration(0, SECONDS)
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
    def s =
      if (delegate == 0) ZeroDuration
      else Duration(delegate, SECONDS)

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

  def randomDuration(maximum: Duration): Duration = Duration(maximum.toNanos * Random.nextFloat(), NANOSECONDS)

  implicit final class RichDuration(private val duration: Duration) extends AnyVal with Ordered[RichDuration]
  {
    def unary_- = ZeroDuration - duration

    def min(o: Duration): Duration =
      if (this <= o) duration else o

    def max(o: Duration): Duration =
      if (this > o) duration else o

    override def toString = pretty  // For ScalaTest

    def msPretty: String = {
      val nanos = duration.toNanos
      if (nanos == 0) "0ms"
      else {
        val a = abs(nanos)
        if (a >= 999_500_000)
          pretty
        else if (a >= 10_000_000)
          formatNumber(nanos / 1_000_000.0, 1, "ms")
        else if (a >= 1_000_000)
          formatNumber(nanos / 1_000_000.0, 10, "ms")
        else if (a >= 10_000)
          formatNumber(nanos / 1_000_000.0, 100, "ms")
        else
          pretty
      }
    }

    def pretty: String =
      duration match {
        case ZeroDuration => "0s"
        case o: FiniteDuration =>
          if (abs(o.toSeconds) < 3*60) smallPretty
          else bigPretty
        case Duration.Inf => "infinite"
        case Duration.Undefined => "undefined"
        case o => o.toString
      }

    private def smallPretty = {
      val nanos = duration.toNanos
      val a = abs(nanos)
      if (a >= 100_000_000_000L)
        formatNumber(nanos / 1000_000_000.0, 1, "s")
      else if (a >= 10_000_000_000L)
        formatNumber(nanos / 1000_000_000.0, 10, "s")
      else if (a >= 1_000_000_000)
        formatNumber(nanos / 1000_000_000.0, 100, "s")
      else if (a >= 100_000_000)
        formatNumber(nanos / 1000_000_000.0, 1000, "s")
      else if (a >= 10_000_000)
        formatNumber(nanos / 1000_000.0, 10, "ms")
      else if (a >= 1_000_000)
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

    private def formatNumber(number: Double, divisor: Int, suffix: String) = {
      val result = new StringBuilder(11 + suffix.length)
      if (number < 0) result += '-'
      val a = floor(abs(number) * divisor + 0.5) / divisor
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

    private def bigPretty = {
      val seconds = duration.toSeconds
      val absSeconds = abs(seconds)
      if (absSeconds >= 22*24*3600)
        s"${seconds / (7*24*3600)}weeks"
      else if (absSeconds >= 3*24*3600)
        bigPretty2(24*3600, "days", 3600, "h")
      else if (absSeconds >= 3600)
        bigPretty2(3600, "h", 60)
      else
        bigPretty2(60, "min", 1)
    }

    private def bigPretty2(primaryFactor: Int, primaryName: String, secondaryFactor: Int, secondaryName: String): String = {
      val seconds = duration.toSeconds
      val sb = new StringBuilder
      val primary = seconds / primaryFactor
      sb.append(primary)
      val secondary = (abs(seconds) - abs(primary * primaryFactor)) / secondaryFactor
      sb.append(primaryName)
      if (secondary > 0) {
        sb.append(secondary)
        sb.append(secondaryName)
      }
      sb.toString
    }

    private def bigPretty2(primaryFactor: Int, primaryName: String, secondaryFactor: Int): String = {
      val seconds = duration.toSeconds
      val sb = new StringBuilder
      val primary = seconds / primaryFactor
      sb.append(primary)
      val secondary = (abs(seconds) - abs(primary * primaryFactor)) / secondaryFactor
      if (secondary > 0) {
        sb.append(f":$secondary%02d")
      }
      sb.append(primaryName)
      sb.toString
    }

    def compare(o: RichDuration) = duration compareTo o.duration
  }

  implicit final class RichFiniteDuration(private val duration: FiniteDuration) extends AnyVal
  {
    def *(o: BigDecimal): FiniteDuration = bigDecimalToDuration(toBigDecimalSeconds * o)
    def /(o: BigDecimal): FiniteDuration = bigDecimalToDuration(toBigDecimalSeconds / o)

    def isZero = duration.length == 0

    def isPositive = duration.length > 0

    def isZeroOrBelow = duration.length <= 0

    def isNegative = duration.length < 0

    def withMillis(milliseconds: Int) =
      if (!duration.isNegative)
        Duration(duration.toSeconds, SECONDS) + milliseconds.ms
      else
        Duration(duration.toSeconds, SECONDS) - milliseconds.ms

    def roundUpToNext(granularity: FiniteDuration): FiniteDuration =
      if (!granularity.isPositive)
        duration
      else {
        val nanos = duration.toNanos
        val sgn = if (nanos >= 0) 1 else -1
        val gran = granularity.toNanos
        Duration((nanos + (gran - 1) * sgn) / gran * gran, NANOSECONDS).toCoarsest
      }

    def toBigDecimalSeconds = duration.unit match {
      case NANOSECONDS  => BigDecimal(duration.length, 9)
      case MICROSECONDS => BigDecimal(duration.length, 6)
      case MILLISECONDS => BigDecimal(duration.length, 3)
      case SECONDS      => BigDecimal(duration.length, 0)
      case MINUTES      => BigDecimal(duration.length) * 60
      case HOURS        => BigDecimal(duration.length) * (60 * 60)
      case DAYS         => BigDecimal(duration.length) * (60 * 60 * 24)
    }

    def toDecimalString: String =
      duration.toBigDecimalSeconds.bigDecimal.toPlainString

    def pretty: String = (duration: Duration).pretty
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
      elapsed max ZeroDuration

    def elapsed: FiniteDuration =
      (System.nanoTime - underlying.time.toNanos).nanoseconds

    def timeLeftOrZero: FiniteDuration =
      underlying.timeLeft max ZeroDuration

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

  implicit val ScalaDurationShow: Show[Duration] = _.pretty
  implicit val FiniteDurationShow: Show[FiniteDuration] = _.pretty
}
