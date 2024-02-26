package js7.base.utils

import cats.effect.{IO, Resource}
import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncVariable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.MutableAllocated.*

final class MutableAllocated[A](using src: sourcecode.Enclosing, tag: Tag[A]):

  private val empty: Allocated[IO, A] = Allocated(null.asInstanceOf[A], IO.unit)
  private val allocatedVar = AsyncVariable(empty)
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
        .update(allocated =>
          if finallyReleased then
            IO.raiseError(new IllegalStateException(
              s"$toString: has been finally released — new aqcuisition rejected"))
          else
            allocated.release *>
              resource.toAllocated)
        .map(_.allocatedThing)

  def release: IO[Unit] =
    logger.traceIO(s"$toString release"):
      allocatedVar
        .update(_.release.as(empty))
        .void

  def finallyRelease: IO[Unit] =
    logger.traceIO(s"$toString finallyRelease"):
      allocatedVar
        .update(_
          .release
          .*>(IO:
            finallyReleased = true)
          .as(empty))
        .void

  override def toString = s"${src.value}: MutableAllocated[${tag.tag}]"


object MutableAllocated:
  private val logger = Logger[this.type]
