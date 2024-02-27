package js7.base.catsutils

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.kernel.Eq
import js7.base.catsutils.SyncDeadline.*
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.FiniteDuration

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
    SyncDeadline(nanosSinceZero + other.toNanos)

  /** Return a deadline moved backwards (i.e., towards the past) by the given duration. */
  def -(other: FiniteDuration): SyncDeadline =
    SyncDeadline(nanosSinceZero - other.toNanos)

  /** Calculate time difference between this and the other deadline, where the result is directed (i.e., may be negative). */
  def -(other: SyncDeadline): FiniteDuration =
    (nanosSinceZero - other.nanosSinceZero).ns

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
    elapsedNanos.ns

  def timeLeftOrZero(using Now): FiniteDuration =
    timeLeft max ZeroDuration

  /** Calculate time difference between this duration and now; the result is negative if the deadline has passed. */
  def timeLeft(using Now): FiniteDuration =
    (-elapsedNanos).ns

  private def elapsedNanos(using now: Now) =
    now.nanosSinceZero - nanosSinceZero

  /** The natural ordering for deadline is determined by the natural order of the underlying (finite) duration. */
  def compare(other: SyncDeadline) =
    (nanosSinceZero - other.nanosSinceZero).compare(0L)

  def toCatsDeadline: CatsDeadline =
    CatsDeadline.fromMonotonicNanos(nanosSinceZero)

  override def toString =
    if nanosSinceZero.abs < toStringTestingLimit then
      // Probably executed for testing
      if nanosSinceZero >= 0 then
        s"SyncDeadline(start+${nanosSinceZero.ns.pretty})"
      else
        s"SyncDeadline(start${nanosSinceZero.ns.pretty})"
    else
      s"SyncDeadline($nanosSinceZero)"


object SyncDeadline:

  // Below this limit, it's probably a testing value and we can show it.
  // Otherwise it's a duration since a random point in time.
  private val toStringTestingLimit = (24.h).toNanos

  final class Now private[SyncDeadline](nanos: Long) extends SyncDeadline(nanos)

  object Now:
    def apply()(using IORuntime): Now =
      fromIORuntime()

    def apply(nanosSinceZero: Long): Now =
      new Now(nanosSinceZero)

    def fromIORuntime()(using ioRuntime: IORuntime): Now =
      fromScheduler()(using ioRuntime.scheduler)

    def fromScheduler()(using scheduler: Scheduler): Now =
      Now(scheduler.monotonicNanos())

    given (using ioRuntime: IORuntime): Now =
      fromIORuntime()

  def now: IO[Now] =
    for d <- IO.monotonic yield
      Now(d.toNanos)

  def now()(using IORuntime): SyncDeadline =
    fromIORuntime()

  def usingNow[A](body: Now ?=> A): IO[A] =
    now.flatMap(now => IO(body(using now)))

  def fromIORuntime()(using ioRuntime: IORuntime): Now =
    fromScheduler()(using ioRuntime.scheduler)

  def fromScheduler()(using scheduler: Scheduler): Now =
    Now(scheduler.monotonicNanos())

  def fromNanos(nanos: Long): SyncDeadline =
    new SyncDeadline(nanos)

  def fromMonotonic(duration: FiniteDuration): SyncDeadline =
    SyncDeadline(duration.toNanos)

  def fromCatsDeadline(catsDeadline: CatsDeadline) =
    fromNanos(catsDeadline.nanosSinceZero)

  //implicit object SyncDeadline2IsOrdered extends Ordering[SyncDeadline]:
  //  def compare(a: SyncDeadline, b: SyncDeadline) = a compare b

  given Eq[SyncDeadline] = _ equals _
