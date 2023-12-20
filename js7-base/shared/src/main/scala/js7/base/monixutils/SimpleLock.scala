package js7.base.monixutils

import cats.effect.Async
import cats.effect.std.Mutex
import cats.syntax.flatMap.*
import js7.base.catsutils.UnsafeMemoizable.given

final class SimpleLock[F[_]](using F: Async[F]):

  private val mutex: F[Mutex[F]] = Mutex[F].unsafeMemoize

  def surround[A](body: F[A]): F[A] =
    mutex.flatMap(_.lock.surround(body))
