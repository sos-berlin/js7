package js7.base.utils

import cats.syntax.parallel.*
import js7.base.monixutils.AsyncVariable
import monix.eval.Task

final class StoppablesRegister
{
  private val stoppables = AsyncVariable[Vector[Stoppable]](Vector.empty)

  def stop: Task[Unit] =
    stoppables
      .update(_
        .parTraverse(_.stop)
        .as(Vector.empty))
      .void

  def add(service: Stoppable): Task[Unit] =
    stoppables
      .update(services => Task.pure(
        if (services.exists(_ eq service))
          services
        else
          services :+ service))
      .void

  def remove(stoppable: Stoppable): Task[Unit] =
    stoppables
      .update(stoppables => Task.pure(
        stoppables.filter(_ ne stoppable)))
      .void
}
