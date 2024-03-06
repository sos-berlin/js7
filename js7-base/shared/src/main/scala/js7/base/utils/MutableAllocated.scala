package js7.base.utils

import cats.effect.{Deferred, IO, Resource}
import izumi.reflect.Tag
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
  private var finallyReleased = false

  def value: IO[A] =
    checked.flatMap:
      case Left(problem) => IO.raiseError(new IllegalStateException(problem.toString))
      case Right(a) => IO.pure(a)

  def checked: IO[Checked[A]] =
    allocatedVar.value.map:
      case `empty` =>
        Left(Problem(s"$toString has not been allocated"))

      case allocated =>
        Right(allocated.allocatedThing)

  def acquire(resource: Resource[IO, A]): IO[A] =
    logger.traceIO(s"$toString acquire"):
      allocatedVar
        .update: allocated =>
          allocated.release.uncancelable
            *> resource.toAllocated
        .map(_.allocatedThing)
        .start.flatMap: fiber =>
          whenReleaseFinally.get
            .guarantee:
              fiber.cancel
            .start.flatMap: cancelFiber =>
              fiber
                .joinWith:
                  IO.defer(IO.raiseError(new AcquisitionCanceledException))
                  //IO.canceled
                  //  .asInstanceOf[IO[A]] /* we have been canceled, so A value is not used. */
                .guaranteeCase: exitCase =>
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
            .*>(IO:
              finallyReleased = true)
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
