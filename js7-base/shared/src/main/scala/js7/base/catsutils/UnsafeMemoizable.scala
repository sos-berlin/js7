package js7.base.catsutils

import cats.effect.syntax.monadCancel.*
import cats.effect.{Async, Deferred, SyncIO}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.utils.{Atomic, Lazy}

/** Typeclass for Monix Task like unsafe memoize for Cats Effect. */
trait UnsafeMemoizable[F[_]]:
  extension[A](x: F[A])
    def unsafeMemoize: F[A]


object UnsafeMemoizable:

  given [F[_]](using F: Async[F]): UnsafeMemoizable[F] with
    extension[A](underlying: F[A])
      def unsafeMemoize: F[A] =
        memoize(underlying)


  // Only to allow IntelliJ to get the proper result type:
  extension [F[_], A](underlying: F[A])(using F: Async[F])
    def unsafeMemoize: F[A] =
      memoize[F, A](underlying)


  // Scala requires a name different from unsafeMemoize.
  /** Same as body·unsafeMemoize, but as a prefix operator. */
  def memoize[F[_], A](body: F[A])(using F: Async[F]): F[A] =
    val triggered = Atomic(false)
    val deferred = Deferred.unsafe[F, Either[Throwable, A]]
    F.defer:
      if triggered.getAndSet(true) then
        deferred.get.flatMap(F.fromEither)
      else
        body
          .attempt
          .flatMap: tried =>
            deferred.complete(tried) >> F.fromEither(tried)
          .uncancelable

  /// SyncIO ///

  given UnsafeMemoizable[SyncIO] with
    extension[A](syncIO: SyncIO[A])
      def unsafeMemoize: SyncIO[A] =
        // `Lazy` blocks the thread when used concurrently !!!
        val lzy = Lazy(syncIO.unsafeRunSync())
        SyncIO.delay(lzy.value)

  extension [A](syncIO: SyncIO[A])
    def unsafeMemoize: SyncIO[A] =
      // `Lazy` blocks the thread when used concurrently !!!
      val lzy = Lazy(syncIO.unsafeRunSync())
      SyncIO.delay(lzy.value)
