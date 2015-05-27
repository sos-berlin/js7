package com.sos.scheduler.engine.common.time

import java.time._
import java.time.temporal.ChronoUnit._
import java.util.concurrent.TimeUnit
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions
import scala.math.abs

object ScalaTime {
  @TestOnly @volatile var extraSleepCount = 0L

  implicit class DurationRichInt(val delegate: Int) extends AnyVal {
    final def ms = Duration.ofMillis(delegate)
    final def s = Duration.ofSeconds(delegate)
    final def h = Duration.ofHours(delegate)
    final def *(o: Duration) =  o multipliedBy delegate
  }

  implicit class DurationRichLong(val delegate: Long) extends AnyVal {
    final def ms = Duration.ofMillis(delegate)
    final def s = Duration.ofSeconds(delegate)
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

  implicit class RichDuration(val delegate: Duration) extends AnyVal {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
    def toBigDecimal = BigDecimal(delegate.getSeconds) + BigDecimal(delegate.getNano) / (1000*1000*1000)
    def toConcurrentDuration: scala.concurrent.duration.Duration = javaToConcurrentDuration(delegate)
    def pretty = millisToPretty(delegate.toMillis)
  }

  implicit class RichInstant(val delegate: Instant) extends AnyVal {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
    def -(o: Instant) = Duration.between(o, delegate)
    def <(o: Instant) = delegate isBefore o
    def <=(o: Instant) = !(delegate isAfter o)
    def >(o: Instant) = delegate isAfter o
    def >=(o: Instant) = !(delegate isBefore o)
  }

  implicit class RichLocalTime(val delegate: LocalTime) extends AnyVal {
    def <(o: LocalTime) = delegate isBefore o
    def <=(o: LocalTime) = !(delegate isAfter o)
    def >(o: LocalTime) = delegate isAfter o
    def >=(o: LocalTime) = !(delegate isBefore o)
  }

  //  implicit class RichDuration(val delegate: Duration) extends AnyVal {
  //    def <(o: Duration) = delegate isShorterThan o
  //    def <=(o: Duration) = !(delegate isLongerThan o)
  //    def >(o: Duration) = delegate isLongerThan o
  //    def >=(o: Duration) = !(delegate isShorterThan o)
  //  }

  implicit object DurationOrdering extends Ordering[Duration] {
    def compare(x: Duration, y: Duration) = x compareTo y
  }

  implicit object InstantOrdering extends Ordering[Instant]{
    def compare(a: Instant, b: Instant) = a compareTo b
  }

  implicit def javaToConcurrentDuration(o: Duration): FiniteDuration =
    new FiniteDuration(o.get(SECONDS), TimeUnit.SECONDS) + new FiniteDuration(o.get(NANOS), TimeUnit.NANOSECONDS)

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
