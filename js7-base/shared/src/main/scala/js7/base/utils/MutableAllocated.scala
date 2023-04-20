package js7.base.utils

import cats.effect.Resource
import izumi.reflect.Tag
import js7.base.monixutils.AsyncVariable
import js7.base.utils.CatsUtils.syntax.RichResource
import monix.eval.Task

final class MutableAllocated[S](implicit S: Tag[S]) {
  private val allocatedVar = AsyncVariable(null.asInstanceOf[Allocated[Task, S]])

  def value: Task[S] =
    allocatedVar.value.flatMap {
      case null =>
        Task.raiseError(new IllegalStateException(s"MutableAllocated[$S] has not been initialized"))

      case allocated =>
        Task.pure(allocated.allocatedThing)
    }

  def use[R](body: S => Task[R]): Task[R] =
    allocatedVar.use {
      case null =>
        Task.raiseError(new IllegalStateException(s"MutableAllocated[$S] has not been initialized"))

      case allocated =>
        body(allocated.allocatedThing)
    }

  def acquire(resource: Resource[Task, S]): Task[S] =
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
}
