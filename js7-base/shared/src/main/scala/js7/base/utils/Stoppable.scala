package js7.base.utils

import cats.effect.Resource
import monix.eval.Task

trait Stoppable
{
  // Idempotent
  def stop: Task[Unit]
}

object Stoppable
{
  def resource[A <: Stoppable](acquire: Task[A]): Resource[Task, A] =
    Resource.make(
      acquire = acquire)(
      release = _.stop)

  implicit final class RichStoppableResource[A](private val resource: Resource[Task, A])
  extends AnyVal
  {
    def acquire(implicit evidence: A <:< Stoppable): Task[A] =
      resource.allocated.map(_._1)
  }
}
