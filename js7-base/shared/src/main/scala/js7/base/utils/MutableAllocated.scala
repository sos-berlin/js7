package js7.base.utils

import cats.effect.{Deferred, IO, ResourceIO}
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.raiseError_
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncVariable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.MutableAllocated.*
import scala.concurrent.CancellationException

final class MutableAllocated[A](logMinor: Boolean = false)
  (using src: sourcecode.Enclosing, tag: Tag[A]):
  self =>

  private val empty: Allocated[IO, A] = Allocated(null.asInstanceOf[A], IO.unit)
  private val allocatedVar = AsyncVariable(empty, logMinor = logMinor)
  private val whenReleaseFinally = Deferred.unsafe[IO, Unit]

  def value: IO[A] =
    checked.flatMap:
      case Left(problem) => IO.raiseError_(new IllegalStateException(problem.toString))
      case Right(a) => IO.pure(a)

  def checked: IO[Checked[A]] =
    allocatedVar.value.map:
      case `empty` =>
        Left(Problem(s"$toString has not been allocated"))

      case allocated =>
        Right(allocated.allocatedThing)

  def acquire(resource: ResourceIO[A]): IO[A] =
    logger.traceIO(s"$toString acquire"):
      cancelWhenFinallyReleased:
        allocatedVar
          .update: allocated =>
            allocated.release.uncancelable *> resource.toAllocated
          .map(_.allocatedThing)

  private def cancelWhenFinallyReleased(io: IO[A]): IO[A] =
    whenReleaseFinally.tryGet.flatMap: released =>
      if released.isDefined then
        IO.raiseError(new AcquisitionCanceledException)
      else
        io.start.flatMap: fiber =>
          whenReleaseFinally.get
            .guarantee:
              fiber.cancel
            .start.flatMap: cancelFiber =>
              fiber
                .joinWith:
                  IO.raiseError_(new AcquisitionCanceledException)
                .guarantee:
                  cancelFiber.cancel

  def release: IO[Unit] =
    logger.traceIO(s"$toString release"):
      allocatedVar
        .update(_.release.as(empty))
        .void

  def releaseFinally: IO[Unit] =
    logger.traceIO(s"$toString releaseFinally"):
      whenReleaseFinally.complete(()) *>
        allocatedVar
          .update(_
            .release
            .as(empty))
          .void

  final class AcquisitionCanceledException private[MutableAllocated]
    extends CancellationException(s"$toString has been finally released — new acquisition rejected")

  override def toString = s"${src.value}: MutableAllocated[${tag.tag}]"


object MutableAllocated:
  private val logger = Logger[this.type]

  def apply[A](using sourcecode.Enclosing, Tag[A]): MutableAllocated[A] =
    apply[A]()

  def apply[A](logMinor: Boolean = false)(using sourcecode.Enclosing, Tag[A]): MutableAllocated[A] =
    new MutableAllocated[A](logMinor = logMinor)
