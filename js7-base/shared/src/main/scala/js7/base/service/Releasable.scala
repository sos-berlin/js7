package js7.base.service

import cats.Functor
import cats.effect.kernel.Sync
import cats.effect.{IO, Resource, ResourceIO}

// Experimental //

/** Something managed by a Resource. */
transparent trait Releasable[F[_]]:
  protected def release: F[Unit]


object Releasable:

  inline def apply[F[_]: Sync as F, R <: Releasable[F]](newReleasable: => R): Resource[F, R] =
    resource(newReleasable)

  inline def apply[F[_]: Functor, R <: Releasable[F]](newReleasable: F[R]): Resource[F, R] =
    resource(newReleasable)

  inline def apply[R <: ReleasableIO](newReleasable: => R): ResourceIO[R] =
    Service.resource(newReleasable)

  inline def apply[R <: ReleasableIO](newReleasable: IO[R]): ResourceIO[R] =
    Service.resource(newReleasable)

  def resource[F[_]: Sync as F, R <: Releasable[F]](newReleasable: => R): Resource[F, R] =
    resource(F.delay(newReleasable))

  def resource[F[_]: Functor, R <: Releasable[F]](newReleasable: F[R]): Resource[F, R] =
    Resource.make(
      acquire = newReleasable)(
      release = _.release)

  inline def resource[R <: ReleasableIO](newReleasable: => R): ResourceIO[R] =
    Service.resource(IO(newReleasable))

  inline def resource[R <: ReleasableIO](newReleasable: IO[R]): ResourceIO[R] =
    Service.resource(newReleasable)


/** A ReleasableIO is a trivial Service, too. */
transparent trait ReleasableIO extends Releasable[IO], Service.TrivialReleasable
