package js7.base.monixutils

import cats.effect.std.Mutex
import cats.effect.{Async, Resource}
import cats.syntax.flatMap.*
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize

final class SimpleLock[F[_]](using F: Async[F]):

  private val mutex = Mutex[F].unsafeMemoize

  def surround[A](body: F[A]): F[A] =
    mutex.flatMap(_.lock.surround(body))

  def resource: Resource[F, Unit] =
    Resource.eval(mutex).flatMap(_.lock)
