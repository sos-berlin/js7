package js7.base.catsutils

import cats.effect.IO
import cats.effect.unsafe.Scheduler
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.{Duration, FiniteDuration, NANOSECONDS}

/** Like Scala's `scala.concurrent.duration.Deadline` but based on Cats Effect's Scheduler. */
final case class SyncDeadline private(nanosSinceZero: Long)(using scheduler: Scheduler)
extends Ordered[SyncDeadline]:

  /**
   * Return a deadline advanced (i.e.s, moved into the future) by the given duration.
   */
  def +(other: FiniteDuration): SyncDeadline =
    copy(nanosSinceZero = nanosSinceZero + other.toNanos)

  /**
   * Return a deadline moved backwards (i.e., towards the past) by the given duration.
   */
  def -(other: FiniteDuration): SyncDeadline =
    copy(nanosSinceZero = nanosSinceZero - other.toNanos)

  /**
   * Calculate time difference between this and the other deadline, where the result is directed (i.e., may be negative).
   */
  def -(other: SyncDeadline): FiniteDuration =
    Duration(nanosSinceZero - other.nanosSinceZero, NANOSECONDS)

  /**
   * Determine whether the deadline lies in the past at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def isOverdue: Boolean =
    hasElapsed

  /**
   * Determine whether the deadline still lies in the future at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def hasTimeLeft: Boolean =
    !hasElapsed

  /** Returns true even if timeLeft == 0, this is different to `timeLeft` */
  def hasElapsed: Boolean =
    elapsedNanos >= 0

  def elapsedOrZero: FiniteDuration =
    elapsed max ZeroDuration

  def elapsed: FiniteDuration =
    Duration(elapsedNanos, NANOSECONDS)

  def timeLeftOrZero: FiniteDuration =
    timeLeft max ZeroDuration

  /**
   * Calculate time difference between this duration and now; the result is negative if the deadline has passed.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def timeLeft: FiniteDuration =
    Duration(-elapsedNanos, NANOSECONDS)

  private def elapsedNanos =
    scheduler.monotonicNanos() - nanosSinceZero

  /**
   * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
   */
  def compare(other: SyncDeadline) =
    (nanosSinceZero - other.nanosSinceZero).compare(0L)

  def toCatsDeadline: CatsDeadline =
    CatsDeadline.fromMonotonicNanos(nanosSinceZero)

  /** Not immutable, may return each nanosecond a different string. */
  override def toString =
    val t = elapsed
    (t.isPositive ?? "+") + t.pretty


object SyncDeadline:

  def fromMonotonicNanos(nanos: Long)(implicit s: Scheduler): SyncDeadline =
    new SyncDeadline(nanos)

  def monotonicClock(using Scheduler): IO[SyncDeadline] =
    IO.delay(now)

  /**
   * Construct a deadline due exactly at the point where this method is called. Useful for then
   * advancing it to obtain a future deadline, or for sampling the current time exactly once and
   * then comparing it to multiple deadlines (using subtraction).
   */
  def now(using scheduler: Scheduler): SyncDeadline =
    SyncDeadline(scheduler.monotonicNanos())

  def fromNanos(nanos: Long)(using Scheduler): SyncDeadline =
    new SyncDeadline(nanos)

  /**
   * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
   */
  implicit object SyncDeadline2IsOrdered extends Ordering[SyncDeadline]:
    def compare(a: SyncDeadline, b: SyncDeadline) = a compare b

  object syntax:
    extension(scheduler: Scheduler)
      def now: SyncDeadline =
        SyncDeadline.now(using scheduler)
