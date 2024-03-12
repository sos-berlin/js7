package js7.base.utils

import cats.effect.Async
import cats.effect.std.Mutex
import cats.syntax.flatMap.*
import js7.base.catsutils.UnsafeMemoizable
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize

final class UnsafeMutex[F[_]: UnsafeMemoizable: Async]:

  private val mutex = Mutex[F].unsafeMemoize

  def lock[A](body: F[A]): F[A] =
    mutex.flatMap(_.lock.surround(body))
