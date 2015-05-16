package com.sos.scheduler.engine.common.time

import java.time._
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
    final def hours = Duration.ofHours(delegate)
    final def days = Duration.ofDays(delegate)
    final def *(o: Duration) = Duration.ofMillis(delegate * o.toMillis)
  }

  implicit class DurationRichLong(val delegate: Long) extends AnyVal {
    final def ms = Duration.ofMillis(delegate)
    final def s = Duration.ofSeconds(delegate)
    final def hours = Duration.ofHours(delegate)
    final def days = Duration.ofDays(delegate)
    final def *(o: Duration) = Duration.ofMillis(delegate * o.toMillis)
  }

  implicit class RichDuration(val delegate: Duration) extends AnyVal {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
    def toScalaDuration = scala.concurrent.duration.Duration(delegate.toMillis, scala.concurrent.duration.MILLISECONDS)
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
    def compare(x: Duration, y: Duration) =
      x.toMillis compare y.toMillis
  }

  implicit object InstantOrdering extends Ordering[Instant]{
    override def compare(a: Instant, b: Instant) = a.toEpochMilli compare b.toEpochMilli
  }

  implicit def jodaToConcurrentDuration(o: Duration): FiniteDuration =
    new FiniteDuration(o.toMillis, TimeUnit.MILLISECONDS)

  def sleep(d: Duration): Unit = {
    sleep(d.toMillis)
  }

  def sleep(millis: Long) = {
    val m = 1000000
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
    if (t < 0) result append '-'
    result append a / 1000
    val tailString = s"000${a % 1000}" takeRight 3
    lengthWithoutTrailingZeros(tailString, tailString.length) match {
      case 0 ⇒
      case n ⇒ result append '.' append tailString.substring(0, n)
    }
    result append 's'
    result.toString()
  }

  @tailrec private def lengthWithoutTrailingZeros(s: String, n: Int): Int =
    n match {
      case 0 ⇒ 0
      case _ if s(n - 1) == '0' => lengthWithoutTrailingZeros(s, n - 1)
      case _ ⇒ n
    }
}
