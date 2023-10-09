package js7.base.catsutils

import cats.effect.concurrent.Deferred
import cats.effect.syntax.bracket.*
import cats.effect.syntax.syncEffect.*
import cats.effect.{Concurrent, SyncEffect, SyncIO, concurrent}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.utils.Lazy

/** Monix Task like unsafe memoize for any Cats Concurrent. */
trait UnsafeMemoizable[F[_]]:
  extension[A](o: F[A])
    def unsafeMemoize: F[A]



object UnsafeMemoizable:

  given concurrentMemoizable[F[_]](using F: Concurrent[F]): UnsafeMemoizable[F] with
    extension[A](leftSide: F[A])
      def unsafeMemoize: F[A] =
        val triggered = AtomicBoolean(false)
        val deferred = Deferred.unsafe[F, Either[Throwable, A]]
        F.defer:
          if !triggered.getAndSet(true) then
            leftSide
              .attempt
              .flatMap(tried =>
                deferred.complete(tried) >> F.fromEither(tried))
              .uncancelable // TODO Allow cancellation, then synchronize `triggered`
          else
            deferred.get.flatMap(F.fromEither)

  given syncEffectMemoizable[F[_]](using F: SyncEffect[F]): UnsafeMemoizable[F] with
    extension[A](leftSide: F[A])
      def unsafeMemoize: F[A] =
        // `Lazy` blocks the thread when used concurrently !!!
        val lzy = Lazy(leftSide.runSync[SyncIO].unsafeRunSync())
        F.delay(lzy.value)
