package js7.base.utils

import cats.effect.{Deferred, FiberIO, IO, Outcome, ResourceIO}
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.raiseError_
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.MutableAllocated.*
import scala.concurrent.CancellationException

/** Holds at most one allocated resource, which can be replaced.
  *
  * `releaseFinally` releases the resource, cancels a concurrent `acquire` and
  * rejects all future calls of `acquire` with an `AcquisitionCanceledException`. */
final class MutableAllocated[A](logMinor: Boolean = false)
  (using src: sourcecode.Enclosing, tag: Tag[A]):
  self =>

  private val empty: Allocated[IO, A] = Allocated(null.asInstanceOf[A], IO.unit)
  private val whenReleaseFinally = Deferred.unsafe[IO, Unit]
  private val lock = AsyncLock(src.value, logMinor = logMinor)
  @volatile private var current: Allocated[IO, A] = empty // Changed only under lock

  def value: IO[A] =
    checked.flatMap:
      case Left(problem) => IO.raiseError_(new IllegalStateException(problem.toString))
      case Right(a) => IO.pure(a)

  def checked: IO[Checked[A]] =
    get.map:
      case None => Left(Problem(s"$toString has not been allocated"))
      case Some(a) => Right(a)

  def get: IO[Option[A]] =
    IO:
      current match
        case `empty` => None
        case allocated => Some(allocated.allocatedThing)

  /** Releases the current resource, then allocates the new one.
    *
    * Fails with `AcquisitionCanceledException` if `releaseFinally` has been called before or
    * while acquiring. */
  def acquire(resource: ResourceIO[A]): IO[A] =
    logger.traceIO(s"$toString acquire"):
      lock.lock:
        // Check under lock, so an `acquire` waiting for the lock is rejected, too
        whenReleaseFinally.tryGet.flatMap:
          case Some(()) =>
            IO.raiseError_(new AcquisitionCanceledException)
          case None =>
            releaseCurrent *>
              allocateUnlessFinallyReleased(resource).map: allocated =>
                current = allocated
                allocated.allocatedThing

  /** Allocates the resource, but cancels the allocation when `releaseFinally` is called.
    *
    * A resource allocated concurrently with the cancellation is released, not lost. */
  private def allocateUnlessFinallyReleased(resource: ResourceIO[A]): IO[Allocated[IO, A]] =
    IO.uncancelable: poll =>
      resource.toAllocated.start.flatMap: fiber =>
        poll:
          IO.race(whenReleaseFinally.get, fiber.join)
        .onCancel:
          cancelAndRelease(fiber)
        .flatMap:
          case Left(()) =>
            cancelAndRelease(fiber) *>
              IO.raiseError_(new AcquisitionCanceledException)
          case Right(Outcome.Succeeded(allocated)) =>
            allocated
          case Right(Outcome.Errored(t)) =>
            IO.raiseError(t)
          case Right(Outcome.Canceled()) =>
            IO.raiseError_(AcquisitionCanceledException:
              s"$toString acquisition has been canceled because releaseFinally has been called concurrently")

  private def cancelAndRelease(fiber: FiberIO[Allocated[IO, A]]): IO[Unit] =
    fiber.cancel *>
      fiber.join.flatMap:
        case Outcome.Succeeded(allocated) => allocated.flatMap(_.release) // Allocated too late
        case _ => IO.unit

  def release: IO[Unit] =
    logger.traceIO(s"$toString release"):
      lock.lock:
        releaseCurrent

  /** Releases the resource, cancels a concurrent `acquire` and rejects future `acquire`s. */
  def releaseFinally: IO[Unit] =
    logger.traceIO(s"$toString releaseFinally"):
      // Complete before locking, to cancel an `acquire` holding the lock
      whenReleaseFinally.complete(()) *>
        lock.lock:
          releaseCurrent

  /** For tests: whether `releaseFinally` has been called. */
  private[utils] def isReleasedFinally: IO[Boolean] =
    whenReleaseFinally.tryGet.map(_.isDefined)

  /** Must be called under lock.
    * The resource is forgotten before being released, so it is never released twice. */
  private def releaseCurrent: IO[Unit] =
    IO.uncancelable: _ =>
      IO.defer:
        val allocated = current
        current = empty
        allocated.release

  final class AcquisitionCanceledException private[MutableAllocated](
    message: String =
      s"${MutableAllocated.this.toString} has been finally released — new acquisition rejected")
  extends
    CancellationException(message)

  override def toString = s"${src.value}: MutableAllocated[${tag.tag}]"


object MutableAllocated:
  private val logger = Logger[this.type]

  def apply[A](using sourcecode.Enclosing, Tag[A]): MutableAllocated[A] =
    apply[A]()

  def apply[A](logMinor: Boolean = false)(using sourcecode.Enclosing, Tag[A]): MutableAllocated[A] =
    new MutableAllocated[A](logMinor = logMinor)
