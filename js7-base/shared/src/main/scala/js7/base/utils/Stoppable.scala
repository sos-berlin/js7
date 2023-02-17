package js7.base.utils

import cats.Functor
import cats.effect.{BracketThrow, Resource}
import cats.syntax.functor.*

trait Stoppable[F[_]]
{
  // Idempotent
  def stop: F[Unit]
}

object Stoppable
{
  def resource[F[_]: Functor, A <: Stoppable[F]](acquire: F[A]): Resource[F, A] =
    Resource.make(
      acquire = acquire)(
      release = _.stop)

  implicit final class RichServiceResource[F[_], A](private val resource: Resource[F, A])
  extends AnyVal
  {
    def acquire(implicit F: BracketThrow[F], evidence: A <:< Stoppable[F]): F[A] =
      resource.allocated.map(_._1)
  }
}
