package js7.base.catsutils

import cats.effect.IO
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.thread.CatsBlocking
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.*
import scala.util.NotGiven

/** Like Scala's `scala.concurrent.duration.Deadline` but based on Cats Effect's Scheduler. */
sealed class CatsDeadline private(val monotonic: FiniteDuration)
extends Ordered[CatsDeadline]:

  override def equals(o: Any): Boolean =
    o match
      case o: CatsDeadline => monotonicNanos == o.monotonicNanos
      case _ => false

  override def hashCode: Int =
    monotonicNanos.hashCode

  def monotonicNanos: Long =
    monotonic.toNanos

  /**
   * Return a deadline advanced (i.e., moved into the future) by the given duration.
   */
  def +(other: FiniteDuration): CatsDeadline =
    CatsDeadline(monotonic + other)

  /**
   * Return a deadline moved backwards (i.e., towards the past) by the given duration.
   */
  def -(other: FiniteDuration): CatsDeadline =
    CatsDeadline(monotonic - other)

  /**
   * Calculate time difference between this and the other deadline, where the result is directed (i.e., may be negative).
   */
  def -(other: CatsDeadline): FiniteDuration =
    monotonic - other.monotonic

  /**
   * Determine whether the deadline lies in the past at the point where this method is called.
   */
  def isOverdue: IO[Boolean] =
    hasElapsed

  /**
   * Determine whether the deadline still lies in the future at the point where this method is called.
   */
  def hasTimeLeft: IO[Boolean] =
    hasElapsed.map(!_)

  /** Returns true even if timeLeft == 0, this is different to `timeLeft` */
  def hasElapsed: IO[Boolean] =
    for o <- elapsed yield !o.isNegative

  def elapsedOrZero: IO[FiniteDuration] =
    for o <- elapsed yield o max ZeroDuration

  def elapsed: IO[FiniteDuration] =
    for o <- timeLeft yield -o

  def timeLeftOrZero: IO[FiniteDuration] =
    for o <- timeLeft yield o max ZeroDuration

  /**
   * Calculate time difference between this duration and now; the result is negative if the deadline has passed.
   */
  def timeLeft: IO[FiniteDuration] =
    for o <- IO.monotonic yield monotonic - o

  def compare(other: CatsDeadline): Int =
    (monotonicNanos - other.monotonicNanos).compare(0L)

  def toDeadline: Deadline =
    Deadline(monotonic)

  // ??? If we had access to IORuntime or Scheduler (like a IO.runtime: IO[IORuntime),
  // we could to a reliant calculation ???
  /** Not immutable, may return a different string for each call.
   *
   * Does work with TestControl, because IO.monotonic will be executed under the outer
   * real (non-test) IORuntime.
   * Then toString calculates the difference to now from two very different clocks.
   */
  override def toString: String =
    //s"CatsDeadline(\"${monotonic.pretty}\")" // Only differences between CatsDeadlines are relevant
    elapsed
      .map(o => (o.isPositive ?? "+") + o.pretty)
      .syncStep(CatsBlocking.SyncStepMaximum)
      .map:
        case Left(_) => "CatsDeadline(?)"
        case Right(o) => o
      .run()


object CatsDeadline:

  val now: IO[Now] =
    for o <- IO.monotonic yield Now(o)

  def usingNow[A](body: Now ?=> A)(/*erase*/ using NotGiven[A <:< IO[?]]): IO[A] =
    now.flatMap(now => IO(body(using Now(now.monotonic))))

  def fromMonotonicNanos(nanos: Long): CatsDeadline =
    fromMonotonic(nanos.ns)

  def fromMonotonic(duration: FiniteDuration): CatsDeadline =
    new CatsDeadline(duration)

  ///**
  // * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
  // */
  //implicit object CatsDeadlineIsOrdered extends Ordering[CatsDeadline]:
  //  def compare(a: CatsDeadline, b: CatsDeadline) = a compare b

  object syntax
  //  implicit final class DeadlineSchedule(private val scheduler: Scheduler) extends AnyVal:
  //    def now: CatsDeadline =
  //      CatsDeadline.now(scheduler)

  final class Now private[CatsDeadline](sinceZero: FiniteDuration) extends CatsDeadline(sinceZero)
