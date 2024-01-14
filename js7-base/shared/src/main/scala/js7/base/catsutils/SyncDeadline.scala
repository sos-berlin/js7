package js7.base.catsutils

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.IO
import js7.base.catsutils.SyncDeadline.*
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.{Duration, FiniteDuration, NANOSECONDS}

/** Like Scala's `scala.concurrent.duration.Deadline` but based on Cats Effect's Scheduler. */
sealed class SyncDeadline private(val nanosSinceZero: Long)
extends Ordered[SyncDeadline]:

  override def equals(o: Any) =
    o match
      case o: SyncDeadline => nanosSinceZero == o.nanosSinceZero
      case _ => false

  override def hashCode =
    nanosSinceZero.hashCode

  /** Return a deadline advanced (i.e.s, moved into the future) by the given duration. */
  def +(other: FiniteDuration): SyncDeadline =
    SyncDeadline(nanosSinceZero = nanosSinceZero + other.toNanos)

  /** Return a deadline moved backwards (i.e., towards the past) by the given duration. */
  def -(other: FiniteDuration): SyncDeadline =
    SyncDeadline(nanosSinceZero = nanosSinceZero - other.toNanos)

  /** Calculate time difference between this and the other deadline, where the result is directed (i.e., may be negative). */
  def -(other: SyncDeadline): FiniteDuration =
    Duration(nanosSinceZero - other.nanosSinceZero, NANOSECONDS)

  /** Determine whether the deadline lies in the past at the point where this method is called. */
  def isOverdue(using Now): Boolean =
    hasElapsed

  /** Determine whether the deadline still lies in the future at the point where this method is called. */
  def hasTimeLeft(using Now): Boolean =
    !hasElapsed

  /** Returns true even if timeLeft == 0, this is different to `timeLeft` */
  def hasElapsed(using Now): Boolean =
    elapsedNanos >= 0

  def elapsedOrZero(using Now): FiniteDuration =
    elapsed max ZeroDuration

  def elapsed(using Now): FiniteDuration =
    Duration(elapsedNanos, NANOSECONDS)

  def timeLeftOrZero(using Now): FiniteDuration =
    timeLeft max ZeroDuration

  /** Calculate time difference between this duration and now; the result is negative if the deadline has passed. */
  def timeLeft(using Now): FiniteDuration =
    Duration(-elapsedNanos, NANOSECONDS)

  private def elapsedNanos(using now: Now) =
    now.nanosSinceZero - nanosSinceZero

  /** The natural ordering for deadline is determined by the natural order of the underlying (finite) duration. */
  def compare(other: SyncDeadline) =
    (nanosSinceZero - other.nanosSinceZero).compare(0L)

  def toCatsDeadline: CatsDeadline =
    CatsDeadline.fromMonotonicNanos(nanosSinceZero)

  //** Not immutable, may return each nanosecond a different string. */
  override def toString = "SyncDeadline"
    //val t = elapsed
    //(t.isPositive ?? "+") + t.pretty


object SyncDeadline:

  final class Now private[SyncDeadline](nanos: Long) extends SyncDeadline(nanos):
    override def toString = "Now"

  def fromNanos(nanos: Long): SyncDeadline =
    new SyncDeadline(nanos)

  def fromScheduler(implicit scheduler: Scheduler): Now =
    Now(scheduler.monotonicNanos())

  def fromIORuntime(implicit ioRuntime: IORuntime): Now =
    fromScheduler(ioRuntime.scheduler)

  def fromCatsDeadline(catsDeadline: CatsDeadline) =
    new SyncDeadline(catsDeadline.nanosSinceZero)

  def now: IO[Now] =
    for d <- IO.monotonic yield
      Now(d.toNanos)

  def use[A](use: Now => A): IO[A] =
    now.map(use)

  implicit object SyncDeadline2IsOrdered extends Ordering[SyncDeadline]:
    def compare(a: SyncDeadline, b: SyncDeadline) = a compare b
