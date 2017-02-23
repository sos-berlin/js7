package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.base.convert.As
import com.sos.scheduler.engine.base.utils.Ascii.isAsciiDigit
import java.time.Instant.now
import java.time._
import java.util.concurrent.TimeUnit
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.math.abs
import scala.util.Random

object ScalaTime {
  val MaxDuration = Duration.ofSeconds(Long.MaxValue, 999999999)
  private val MaxConcurrentDuration = Duration.ofNanos(Long.MaxValue)
  val Iso8601DurationRegex = "[0-9.A-Za-z]+".r

  @TestOnly @volatile var extraSleepCount = 0L

  implicit class DurationRichInt(val delegate: Int) extends AnyVal {
    /**
     * Duration, counted in microseconds.
     */
    final def µs = Duration ofNanos 1000L * delegate

    /**
     * Duration, counted in milliseconds.
     */
    final def ms = Duration ofMillis delegate

    /**
     * Duration, counted in seconds.
     */
    final def s = Duration ofSeconds delegate

    // Conflicts with scala.runtime.RichInt.min
    ///**
    // * Duration, counted in minutes.
    // */
    //final def min = Duration ofMinutes delegate

    /**
     * Duration, counted in hours.
     */
    final def h = Duration ofHours delegate

    final def *(o: Duration) =  o multipliedBy delegate
  }

  implicit class DurationRichLong(val delegate: Long) extends AnyVal {
    /**
     * Duration, counted in microseconds.
     */
    final def µs = Duration ofNanos 1000L * delegate

    /**
     * Duration, counted in milliseconds.
     */
    final def ms = Duration ofMillis delegate

    /**
     * Duration, counted in seconds.
     */
    final def s = Duration ofSeconds delegate

    // Conflicts with scala.runtime.RichLong.min
    ///**
    // * Duration, counted in minutes.
    // */
    //final def min = Duration ofMinutes delegate

    /**
     * Duration, counted in hours.
     */
    final def h = Duration ofHours delegate

    final def *(o: Duration) = o multipliedBy delegate
  }

  implicit class DurationRichBigDecimal(val delegate: BigDecimal) extends AnyVal {
    final def s: Duration = bigDecimalToDuration(delegate)
  }

  def bigDecimalToDuration(o: BigDecimal) = {
    val (seconds, nanos) = o /% 1
    Duration.ofSeconds(seconds.toLongExact, (nanos * 1000*1000*1000).toIntExact)
  }

  implicit val StringAsDuration: As[String, Duration] = As(parseDuration)

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
    Duration parse (
      if (string.nonEmpty && isAsciiDigit(string.head))
        if (isAsciiDigit(string.last))
          s"PT${string}S"
        else
          s"PT$string"
      else
        string)

  def randomDuration(maximum: Duration): Duration = Duration ofNanos (maximum.toNanos * Random.nextFloat).toLong

  implicit class RichDuration(val delegate: Duration) extends AnyVal with Ordered[RichDuration] {
    def unary_- = Duration.ZERO minus delegate
    def +(o: Duration): Duration = delegate plus o
    def -(o: Duration): Duration = delegate minus o
    def *(o: Int): Duration = delegate multipliedBy o
    def /(o: Int): Duration = delegate dividedBy o
    def *(o: BigDecimal): Duration = bigDecimalToDuration(delegate.toBigDecimal * o)
    def /(o: BigDecimal): Duration = bigDecimalToDuration(delegate.toBigDecimal / o)
    def min(o: Duration): Duration = if (this <= o) delegate else o
    def max(o: Duration): Duration = if (this > o) delegate else o
    def toBigDecimal = BigDecimal(delegate.getSeconds) + BigDecimal(delegate.getNano) / (1000*1000*1000)
    def toConcurrent: scala.concurrent.duration.Duration = javaToConcurrentDuration(delegate)
    def toFiniteDuration: scala.concurrent.duration.FiniteDuration = javaToConcurrentFiniteDuration(delegate)
    override def toString = pretty  // For ScalaTest

    def pretty =
      if (delegate == Duration.ZERO) "0s"
      else {
        val seconds = delegate.getSeconds
        val absSeconds = abs(seconds)
        if (absSeconds >= 3*366*24*60*60) s"${seconds / (366*24*60*60)}~years"
        else if (absSeconds >= 366*24*60*60) s"${seconds / (30*24*60*60)}~months"
        else if (absSeconds >= 3*24*60*60) s"${seconds / (24*60*60)}d"
        else if (absSeconds >= 3*60*60) s"${seconds / (60*60)}h"
        else if (absSeconds >= 3*60) s"${seconds / 60}min"
        else {
          val millis = delegate.toMillis
          val nanos = delegate.toNanos
          if (abs(millis) >= 10 || millis >= 1 && nanos / 1000 % 1000 == 0) millisToPretty(millis)
          else {
            if (abs(nanos) > 10000) s"${delegate.toNanos / 1000}µs"
            else s"${delegate.toNanos}ns"
          }
        }
      }

    def toSecondsString: String = delegate.toBigDecimal.bigDecimal.toPlainString + "s"

    def compare(o: RichDuration) = delegate compareTo o.delegate
  }

  implicit class RichInstant(val delegate: Instant) extends AnyVal with Ordered[RichInstant] {
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

  implicit class RichLocalTime(val delegate: LocalTime) extends AnyVal with Ordered[RichLocalTime] {
    def compare(o: RichLocalTime) = delegate compareTo o.delegate
  }

//  implicit object InstantOrdering extends Ordering[Instant] {
//    def compare(a: Instant, b: Instant) = a compareTo b
//  }
//
//  implicit object LocalTimeOrdering extends Ordering[LocalTime] {
//    def compare(a: LocalTime, b: LocalTime) = a compareTo b
//  }

  implicit class RichLocalDateTime(val delegate: LocalDateTime) extends AnyVal with Ordered[RichLocalDateTime] {
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

  private def simpleJavaToConcurrentFiniteDuration(o: Duration) =
    new FiniteDuration(o.toNanos, TimeUnit.NANOSECONDS).toCoarsest.asInstanceOf[FiniteDuration]

  @tailrec
  def sleepUntil(until: Instant): Unit = {
    val duration = until - now()
    if (duration > 0.s) {
      sleep(duration)
      sleepUntil(until)
    }
  }

  def sleep(d: Duration): Unit = sleep(d.toMillis)

  def sleep(millis: Long) = {
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

  def millisToPretty(t: Long) = {
    val result = new StringBuilder(30)
    val a = abs(t)
    if (t < 0) result += '-'
    result.append(a / 1000)
    val tailString = s"000${a % 1000}" takeRight 3
    lengthWithoutTrailingZeros(tailString, tailString.length) match {
      case 0 ⇒
      case n ⇒ result.append('.').append(tailString.substring(0, n))
    }
    result += 's'
    result.toString()
  }

  @tailrec private def lengthWithoutTrailingZeros(s: String, n: Int): Int =
    n match {
      case 0 ⇒ 0
      case _ if s(n - 1) == '0' => lengthWithoutTrailingZeros(s, n - 1)
      case _ ⇒ n
    }

  def dateToInstant(date: java.util.Date): Instant = Instant.ofEpochMilli(date.getTime)
}
