package js7.base.utils

import cats.syntax.parallel.*
import js7.base.monixutils.AsyncVariable
import monix.eval.Task

final class StoppablesRegister
{
  private val stoppables = AsyncVariable[Vector[Stoppable[Task]]](Vector.empty)

  def stop: Task[Unit] =
    stoppables
      .update(_
        .parTraverse(_.stop)
        .as(Vector.empty))
      .void

  def add(stoppable: Stoppable[Task]): Task[Unit] =
    stoppables
      .update(stoppables => Task.pure(
        if (stoppables.exists(_ eq stoppable))
          stoppables
        else
          stoppables :+ stoppable))
      .void

  def remove(stoppable: Stoppable[Task]): Task[Unit] =
    stoppables
      .update(stoppables => Task.pure(
        stoppables.filter(_ ne stoppable)))
      .void
}
