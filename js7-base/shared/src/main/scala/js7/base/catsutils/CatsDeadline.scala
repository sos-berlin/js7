package js7.base.catsutils

import cats.effect.IO
import cats.effect.kernel.Clock
import js7.base.catsutils.CatsDeadline
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.*

/** Like Scala's `scala.concurrent.duration.Deadline` but based on Monix' clock. */
final case class CatsDeadline private(sinceZero: FiniteDuration):
//extends Ordered[CatsDeadline]

  /**
   * Return a deadline advanced (i.e., moved into the future) by the given duration.
   */
  def +(other: FiniteDuration): CatsDeadline =
    copy(sinceZero = sinceZero + other)

  /**
   * Return a deadline moved backwards (i.e., towards the past) by the given duration.
   */
  def -(other: FiniteDuration): CatsDeadline =
    copy(sinceZero = sinceZero - other)

  /**
   * Calculate time difference between this and the other deadline, where the result is directed (i.e., may be negative).
   */
  def -(other: CatsDeadline): FiniteDuration =
    sinceZero - other.sinceZero

  /**
   * Determine whether the deadline lies in the past at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def isOverdue: IO[Boolean] =
    hasElapsed

  /**
   * Determine whether the deadline still lies in the future at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def hasTimeLeft: IO[Boolean] =
    hasElapsed.map(!_)

  /** Returns true even if timeLeft == 0, this is different to `timeLeft` */
  def hasElapsed: IO[Boolean] =
    for o <- IO.monotonic yield !o.isNegative

  def elapsedOrZero: IO[FiniteDuration] =
    for o <- elapsed yield o max ZeroDuration

  def elapsed: IO[FiniteDuration] =
    for o <- IO.monotonic yield o - sinceZero

  def timeLeftOrZero: IO[FiniteDuration] =
    for o <- timeLeft yield o max ZeroDuration

  /**
   * Calculate time difference between this duration and now; the result is negative if the deadline has passed.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def timeLeft: IO[FiniteDuration] =
    for o <- elapsed yield -o

  ///**
  // * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
  // */
  //def compare(other: CatsDeadline) =
  //  sinceZero compare other.sinceZero

  def now: IO[CatsDeadline] =
    for o <- IO.monotonic yield CatsDeadline(o)

  /** Not immutable, may return a different string for each call. */
  override def toString =
    (for o <- elapsed yield (o.isPositive ?? "+") + o.pretty)
      .syncStep(limit = 1000)
      .map:
        case Left(_) => "Deadline"
        case Right(o) => o


object CatsDeadline:
  val now: IO[CatsDeadline] =
    for o <- IO.monotonic yield CatsDeadline(o)

  @deprecated
  val monotonicClock: IO[CatsDeadline] =
    now
  //  IO.deferAction { scheduler =>
  //    IO.pure(CatsDeadline.now(scheduler))
  //  }

  //def fromNanos(sinceZero: Long)(implicit s: Scheduler): CatsDeadline =
  //  new CatsDeadline(sinceZero.ns)

  ///**
  // * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
  // */
  //implicit object CatsDeadlineIsOrdered extends Ordering[CatsDeadline]:
  //  def compare(a: CatsDeadline, b: CatsDeadline) = a compare b

  object syntax
  //  implicit final class DeadlineSchedule(private val scheduler: Scheduler) extends AnyVal:
  //    def now: CatsDeadline =
  //      CatsDeadline.now(scheduler)
