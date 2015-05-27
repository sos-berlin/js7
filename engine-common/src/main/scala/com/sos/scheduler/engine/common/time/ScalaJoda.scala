package com.sos.scheduler.engine.common.time

import java.util.concurrent.TimeUnit
import org.joda.time.Duration.{millis, standardDays, standardHours, standardSeconds}
import org.joda.time._
import org.joda.time.base.AbstractInstant
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions
import scala.math.abs

object ScalaJoda {
  @volatile var extraSleepCount = 0L

  implicit class DurationRichInt(val delegate: Int) extends AnyVal {
    final def ms = millis(delegate)
    final def s = standardSeconds(delegate)
    final def hours = standardHours(delegate)
    final def days = standardDays(delegate)
    final def *(o: Duration) = millis(delegate * o.getMillis)
  }

  implicit class DurationRichLong(val delegate: Long) extends AnyVal {
    final def ms = millis(delegate)
    final def s = standardSeconds(delegate)
    final def hours = standardHours(delegate)
    final def days = standardDays(delegate)
    final def *(o: Duration) = millis(delegate * o.getMillis)
  }

  implicit class RichDuration(val delegate: Duration) extends AnyVal {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
  }

  implicit class RichReadableDuration(val delegate: ReadableDuration) extends AnyVal {
    def toScalaDuration = scala.concurrent.duration.Duration(delegate.getMillis, scala.concurrent.duration.MILLISECONDS)
    def toJava = java.time.Duration.ofMillis(delegate.getMillis)
    def pretty = millisToPretty(delegate.getMillis)
  }

  implicit class RichInstant(val delegate: Instant) extends AnyVal {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
    def -(o: Instant) = new Duration(o, delegate)
  }

  implicit class RichDateTime(val delegate: DateTime) extends AnyVal {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
    def -(o: DateTime) = new Duration(o, delegate)
  }

  implicit class RichAbstractInstant(val delegate: AbstractInstant) extends AnyVal {
    def <(o: AbstractInstant) = delegate isBefore o
    def <=(o: AbstractInstant) = !(delegate isAfter o)
    def >(o: AbstractInstant) = delegate isAfter o
    def >=(o: AbstractInstant) = !(delegate isBefore o)
    def toJava = java.time.Instant.ofEpochMilli(delegate.getMillis)
  }

  implicit class RichLocalTime(val delegate: LocalTime) extends AnyVal {
    def <(o: LocalTime) = delegate isBefore o
    def <=(o: LocalTime) = !(delegate isAfter o)
    def >(o: LocalTime) = delegate isAfter o
    def >=(o: LocalTime) = !(delegate isBefore o)
  }

//  implicit class RichDuration(val delegate: ReadableDuration) extends AnyVal {
//    def <(o: ReadableDuration) = delegate isShorterThan o
//    def <=(o: ReadableDuration) = !(delegate isLongerThan o)
//    def >(o: ReadableDuration) = delegate isLongerThan o
//    def >=(o: ReadableDuration) = !(delegate isShorterThan o)
//  }

  implicit object ReadableDurationOrdering extends Ordering[ReadableDuration] {
    def compare(x: ReadableDuration, y: ReadableDuration) =
      x.getMillis compare y.getMillis
  }

  implicit object DurationOrdering extends Ordering[Duration] {
    def compare(x: Duration, y: Duration) =
      x.getMillis compare y.getMillis
  }

  implicit object InstantOrdering extends Ordering[Instant]{
    override def compare(a: Instant, b: Instant) = a.getMillis compare b.getMillis
  }

  implicit def jodaToConcurrentDuration(o: Duration): FiniteDuration =
    new FiniteDuration(o.getMillis, TimeUnit.MILLISECONDS)

  def sleep(d: Duration): Unit = {
    sleep(d.getMillis)
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
      case 0 =>
      case n => result append '.' append tailString.substring(0, n)
    }
    result append 's'
    result.toString()
  }

  @tailrec private def lengthWithoutTrailingZeros(s: String, n: Int): Int = n match {
    case 0 => 0
    case _ if s(n - 1) == '0' => lengthWithoutTrailingZeros(s, n - 1)
    case _ => n
  }
}
