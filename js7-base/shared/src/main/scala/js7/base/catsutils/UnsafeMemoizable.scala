package js7.base.catsutils

import cats.Defer
import cats.effect.kernel.{Async, Deferred, Sync}
import cats.effect.syntax.monadCancel.*
import cats.effect.{Concurrent, IO, SyncIO}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.utils.{Atomic, Lazy}

/** Typeclass for Monix Task like unsafe memoize for Cats Effect. */
trait UnsafeMemoizable[F[_]]:
  extension[A](o: F[A]) def unsafeMemoize: F[A]


object UnsafeMemoizable:

  given [F[_]]: UnsafeMemoizable[F] with
    extension[A](underlying: F[A])
      def unsafeMemoize(using F: Async[F]): F[A] =
        val triggered = Atomic(false)
        val deferred = Deferred.unsafe[F, Either[Throwable, A]]
        F.defer:
          if triggered.getAndSet(true) then
            deferred.get.flatMap(F.fromEither)
          else
            underlying
              .attempt
              .flatMap(tried =>
                deferred.complete(tried) >> F.fromEither(tried))
              .uncancelable // TODO Allow cancellation, then synchronize `triggered`

  // Explicit implementation for IO required ???
  //given ioMemoizable: UnsafeMemoizable[IO] with
  //  extension[A](io: IO[A])
  //    def unsafeMemoize: IO[A] =
  //      val triggered = Atomic(false)
  //      val deferred = Deferred.unsafe[IO, Either[Throwable, A]]
  //      IO.defer:
  //        if triggered.getAndSet(true) then
  //          deferred.get.flatMap(F.fromEither)
  //        else
  //          io
  //            .attempt
  //            .flatMap(tried =>
  //              deferred.complete(tried) >> IO.fromEither(tried))
  //            .uncancelable // TODO Allow cancellation, then synchronize `triggered`

  given UnsafeMemoizable[SyncIO] with
    extension[A](syncIO: SyncIO[A])
      def unsafeMemoize: SyncIO[A] =
        // `Lazy` blocks the thread when used concurrently !!!
        val lzy = Lazy(syncIO.unsafeRunSync())
        SyncIO.delay(lzy.value)
