package js7.base.utils

import cats.effect.Resource
import izumi.reflect.Tag
import js7.base.monixutils.AsyncVariable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.syntax.RichResource
import monix.eval.Task

final class MutableAllocated[A](implicit src: sourcecode.Enclosing, tag: Tag[A]) {
  private val allocatedVar = AsyncVariable(null.asInstanceOf[Allocated[Task, A]])

  def value: Task[A] =
    checked.flatMap {
      case Left(problem) => Task.raiseError(new IllegalStateException(problem.toString))
      case Right(a) => Task.pure(a)
    }

  def checked: Task[Checked[A]] =
    allocatedVar.value.map {
      case null =>
        Left(Problem(s"$toString has not been initialized"))

      case allocated =>
        Right(allocated.allocatedThing)
    }

  def acquire(resource: Resource[Task, A]): Task[A] =
    allocatedVar
      .update(allocated =>
        Task.when(allocated != null)(allocated.stop) *>
          resource.toAllocated)
      .map(_.allocatedThing)

  def release: Task[Unit] =
    allocatedVar.value.flatMap {
      case null => Task.unit
      case o => o.stop
    }

  override def toString = s"${src.value}: MutableAllocated[${tag.tag}]"
}
