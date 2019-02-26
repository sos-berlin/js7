package com.sos.jobscheduler.common.time

import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Ascii.isAsciiDigit
import java.time.Instant.now
import java.time._
import java.util.concurrent.TimeUnit
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec
import scala.concurrent.duration.{FiniteDuration, Duration => ScalaDuration}
import scala.language.implicitConversions
import scala.math.abs
import scala.util.Random

object ScalaTime {
  val MaxDuration = Duration.ofSeconds(Long.MaxValue, 999999999)
  private val MaxConcurrentDuration = Duration.ofNanos(Long.MaxValue)
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

  def bigDecimalToDuration(o: BigDecimal) = {
    val (seconds, fraction) = o /% 1
    try Duration.ofSeconds(seconds.toLongExact, (fraction * 1000*1000*1000).toIntExact)
    catch { case t: ArithmeticException =>
      throw new ArithmeticException(s"Not a Duration (${t.getMessage}): $o")
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

  def randomDuration(maximum: Duration): Duration = Duration ofNanos (maximum.toNanos * Random.nextFloat).toLong

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
    def toConcurrent: scala.concurrent.duration.Duration = javaToConcurrentDuration(delegate)
    def toFiniteDuration: scala.concurrent.duration.FiniteDuration = javaToConcurrentFiniteDuration(delegate)
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
        nanos + "ns"
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

    def toSecondsString: String = delegate.toBigDecimal.bigDecimal.toPlainString + "s"

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

    def toTimestamp = Timestamp.ofEpochMilli(delegate.toEpochMilli)

    override def toString = delegate.toString  // For ScalaTest
  }

  implicit final class RichLocalTime(private val delegate: LocalTime) extends AnyVal with Ordered[RichLocalTime] {
    def compare(o: RichLocalTime) = delegate compareTo o.delegate
  }

//  implicit object InstantOrdering extends Ordering[Instant] {
//    def compare(a: Instant, b: Instant) = a compareTo b
//  }
//
//  implicit object LocalTimeOrdering extends Ordering[LocalTime] {
//    def compare(a: LocalTime, b: LocalTime) = a compareTo b
//  }

  implicit final class RichLocalDateTime(private val delegate: LocalDateTime) extends AnyVal with Ordered[RichLocalDateTime] {
    def compare(o: RichLocalDateTime) = delegate compareTo o.delegate
    def toInstant(zone: ZoneId) = delegate.toInstant(zone.getRules.getOffset(delegate))
  }

  def javaToConcurrentDuration(o: Duration): scala.concurrent.duration.Duration = {
    if ((o compareTo MaxConcurrentDuration) > 0) scala.concurrent.duration.Duration.Inf
    else simpleJavaToConcurrentFiniteDuration(o)
  }

  def javaToConcurrentFiniteDuration(o: Duration): FiniteDuration = {
    if ((o compareTo Duration.ofNanos(Long.MaxValue)) > 0) FiniteDuration(Long.MaxValue, TimeUnit.NANOSECONDS)
    else simpleJavaToConcurrentFiniteDuration(o)
  }

  private def simpleJavaToConcurrentFiniteDuration(o: Duration): FiniteDuration =
    new FiniteDuration(o.toNanos, TimeUnit.NANOSECONDS).toCoarsest

  @tailrec
  def sleepUntil(until: Instant): Unit = {
    val duration = until - now()
    if (duration > 0.s) {
      sleep(duration)
      sleepUntil(until)
    }
  }

  def sleep(d: Duration): Unit = sleep(d.toMillis)

  def sleep(millis: Long) =
    if (millis > 0) {
      val m = 1000*1000
      val until = System.nanoTime() + millis * m
      Thread.sleep(millis)
      @tailrec def extraSleep(): Unit = {
        val remainingNanos = until - System.nanoTime()
        if (remainingNanos > 0) {
          extraSleepCount += 1
          Thread.sleep(remainingNanos / m, (remainingNanos % m).toInt)
          extraSleep()
        }
      }
      extraSleep()
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

  implicit final class RichFiniteDuration(private val underlying: FiniteDuration) extends AnyVal {
    def toJavaDuration = Duration.ofNanos(underlying.toNanos)
  }

  implicit final class RichConcurrentDuration(private val underlying: ScalaDuration) extends AnyVal {
    def pretty: String =
      underlying match {
        case o: FiniteDuration => o.toJavaDuration.pretty
        case ScalaDuration.Inf => "infinite"
        case ScalaDuration.Undefined => "undefined"
        case o => o.toString
      }
  }

  implicit final class RichTimestamp(private val underlying: Timestamp) extends AnyVal {
    def toInstant = Instant.ofEpochMilli(underlying.toEpochMilli)
  }

  implicit def finiteToJavaDuration(duration: FiniteDuration): Duration =
    duration.toJavaDuration
}
