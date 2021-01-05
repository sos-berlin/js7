package js7.base.monixutils

import js7.base.monixutils.MonixDeadline._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import monix.execution.Scheduler
import scala.concurrent.duration.{Duration, FiniteDuration, NANOSECONDS}

/** Like Scala's `scala.concurrent.duration.Deadline` but based on Monix' clock. */
final case class MonixDeadline private(nanos: Long)(implicit scheduler: Scheduler)
extends Ordered[MonixDeadline]
{
  /**
   * Return a deadline advanced (i.e., moved into the future) by the given duration.
   */
  def +(other: FiniteDuration): MonixDeadline =
    copy(nanos = nanos + other.toNanos)

  /**
   * Return a deadline moved backwards (i.e., towards the past) by the given duration.
   */
  def -(other: FiniteDuration): MonixDeadline =
    copy(nanos = nanos - other.toNanos)

  /**
   * Calculate time difference between this and the other deadline, where the result is directed (i.e., may be negative).
   */
  def -(other: MonixDeadline): FiniteDuration =
    Duration(nanos - other.nanos, NANOSECONDS)

  /** Returns true even if timeLeft == 0, this is different to `timeLeft` */
  def hasElapsed: Boolean =
    nanos <= nowNanos

  def elapsedOrZero: FiniteDuration =
    elapsed max Duration.Zero

  def elapsed: FiniteDuration =
    Duration(nowNanos - nanos, NANOSECONDS)

  def timeLeftOrZero: FiniteDuration =
    timeLeft max Duration.Zero

  /**
   * Calculate time difference between this duration and now; the result is negative if the deadline has passed.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def timeLeft: FiniteDuration =
    Duration(nanos - nowNanos, NANOSECONDS)

  /**
   * Determine whether the deadline still lies in the future at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def hasTimeLeft: Boolean =
    (nanos - nowNanos) > 0

  /**
   * Determine whether the deadline lies in the past at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def isOverdue: Boolean =
    (nanos - nowNanos) < 0

  /**
   * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
   */
  def compare(other: MonixDeadline) =
    nanos compare other.nanos

  /** Not immutable, may return each nanosecond a different string. */
  override def toString = {
    val t = elapsed
    ((t > 0.s) ?? "+") + t.toCoarsest.toString
  }
}

object MonixDeadline
{
  /**
   * Construct a deadline due exactly at the point where this method is called. Useful for then
   * advancing it to obtain a future deadline, or for sampling the current time exactly once and
   * then comparing it to multiple deadlines (using subtraction).
   */
  def now(implicit s: Scheduler): MonixDeadline =
    MonixDeadline(nowNanos(s))

  private def nowNanos(implicit scheduler: Scheduler): Long =
    scheduler.clockMonotonic(NANOSECONDS)

  def fromNanos(nanos: Long)(implicit s: Scheduler): MonixDeadline =
    new MonixDeadline(nanos)

  /**
   * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
   */
  implicit object MonixDeadlineIsOrdered extends Ordering[MonixDeadline] {
    def compare(a: MonixDeadline, b: MonixDeadline) = a compare b
  }

  object syntax
  {
    implicit final class DeadlineSchedule(private val underlying: Scheduler) extends AnyVal
    {
      def now: MonixDeadline =
        MonixDeadline.now(underlying)
    }
  }
}
