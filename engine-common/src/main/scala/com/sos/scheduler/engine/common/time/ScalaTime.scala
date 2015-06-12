package com.sos.scheduler.engine.common.time

import java.time._
import java.util.concurrent.TimeUnit
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
//import scala.language.implicitConversions
import scala.math.abs

object ScalaTime {
  val MaxDuration = Duration.ofSeconds(Long.MaxValue, 999999999)
  private val MaxConcurrentDuration = Duration.ofNanos(Long.MaxValue)
  @TestOnly @volatile var extraSleepCount = 0L

  implicit class DurationRichInt(val delegate: Int) extends AnyVal {
    /**
     * Duration, counted in milliseconds.
     */
    final def ms = Duration.ofMillis(delegate)

    /**
     * Duration, counted in seconds.
     */
    final def s = Duration.ofSeconds(delegate)

    /**
     * Duration, counted in hours.
     */
    final def h = Duration.ofHours(delegate)

    final def *(o: Duration) =  o multipliedBy delegate
  }

  implicit class DurationRichLong(val delegate: Long) extends AnyVal {
    /**
     * Duration, counted in milliseconds.
     */
    final def ms = Duration.ofMillis(delegate)

    /**
     * Duration, counted in seconds.
     */
    final def s = Duration.ofSeconds(delegate)

    /**
     * Duration, counted in hours.
     */
    final def h = Duration.ofHours(delegate)

    final def *(o: Duration) = o multipliedBy delegate
  }

  implicit class DurationRichBigDecimal(val delegate: BigDecimal) extends AnyVal {
    final def s: Duration = bigDecimalToDuration(delegate)
  }

  def bigDecimalToDuration(o: BigDecimal) = {
    val (seconds, nanos) = o /% 1
    Duration.ofSeconds(seconds.toLongExact, (nanos * 1000*1000*1000).toIntExact)
  }

  implicit class RichDuration(val delegate: Duration) extends AnyVal with Ordered[RichDuration] {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
    def toBigDecimal = BigDecimal(delegate.getSeconds) + BigDecimal(delegate.getNano) / (1000*1000*1000)
    def toConcurrent: scala.concurrent.duration.Duration = javaToConcurrentDuration(delegate)
    def toFiniteDuration: scala.concurrent.duration.FiniteDuration = javaToConcurrentFiniteDuration(delegate)
    def pretty = millisToPretty(delegate.toMillis)
    def compare(o: RichDuration) = delegate compareTo o.delegate
  }

  implicit class RichInstant(val delegate: Instant) extends AnyVal with Ordered[RichInstant] {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
    def -(o: Instant) = Duration.between(o, delegate)
    def compare(o: RichInstant) = delegate compareTo o.delegate
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
}
