package js7.base.utils

import cats.effect.Resource
import izumi.reflect.Tag
import js7.base.monixutils.AsyncVariable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.syntax.RichResource
import monix.eval.Task

final class MutableAllocated[A](implicit src: sourcecode.Enclosing, tag: Tag[A]) {
  private val empty: Allocated[Task, A] = Allocated(null.asInstanceOf[A], Task.unit)
  private val allocatedVar = AsyncVariable(empty)
  private var finallyReleased = false

  def value: Task[A] =
    checked.flatMap {
      case Left(problem) => Task.raiseError(new IllegalStateException(problem.toString))
      case Right(a) => Task.pure(a)
    }

  def checked: Task[Checked[A]] =
    allocatedVar.value.map {
      case `empty` =>
        Left(Problem(s"$toString has not been allocated"))

      case allocated =>
        Right(allocated.allocatedThing)
    }

  def acquire(resource: Resource[Task, A]): Task[A] =
    allocatedVar
      .update(allocated =>
        if finallyReleased then
          Task.raiseError(new IllegalStateException(
            s"$toString: has been finally released — new aqcuisition rejected"))
        else
          allocated.release *>
            resource.toAllocated)
      .map(_.allocatedThing)

  def release: Task[Unit] =
    allocatedVar
      .update(_.release.as(empty))
      .void

  def finallyRelease: Task[Unit] =
    allocatedVar
      .update(_
        .release
        .*>(Task {
          finallyReleased = true
        })
        .as(empty))
      .void

  override def toString = s"${src.value}: MutableAllocated[${tag.tag}]"
}
