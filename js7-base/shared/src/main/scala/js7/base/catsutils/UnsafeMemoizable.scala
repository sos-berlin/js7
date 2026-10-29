package js7.base.catsutils

import cats.effect.syntax.monadCancel.*
import cats.effect.{Async, Deferred, SyncIO}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.utils.{Atomic, Lazy}
import scala.concurrent.CancellationException

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
  extension [F[_]: Async as F, A](underlying: F[A])
    def unsafeMemoize: F[A] =
      memoize[F, A](underlying)


  // Scala requires a name different from unsafeMemoize.
  /** Same as body·unsafeMemoize, but as a prefix operator.
    *
    * `body` is evaluated at most once, and its result (or failure) is shared by all callers.
    *
    * `body` runs in a fiber of its own, which no caller can cancel. So every caller,
    * including the first one, can be canceled while waiting, without affecting the evaluation.
    * The evaluation continues, even if all callers have been canceled, and is available
    * for later callers.
    * (A `body` that never completes leaves its fiber behind, until it is garbage-collected.) */
  def memoize[F[_]: Async as F, A](body: F[A]): F[A] =
    val triggered = Atomic(false)
    val result = Atomic(none[Either[Throwable, A]]) // Fast path after evaluation
    val deferred = Deferred.unsafe[F, Either[Throwable, A]]

    def complete(tried: Either[Throwable, A]): F[Unit] =
      F.delay(result.set(Some(tried))) >>
        deferred.complete(tried).void

    val evaluate: F[Unit] =
      body.attempt.flatMap(complete)
        .onCancel:
          // The body has canceled itself, because no one else cancels this fiber.
          // Without this, the callers would wait forever
          complete(Left(new CancellationException("memoized evaluation has been canceled")))

    val await: F[A] =
      deferred.get.flatMap(F.fromEither)

    F.defer:
      result.get match
        case Some(tried) =>
          F.fromEither(tried)

        case None =>
          // Atomic regarding cancellation: the evaluation must start if `triggered` is set,
          // otherwise later callers would wait forever.
          F.uncancelable: poll =>
            if triggered.getAndSet(true) then
              poll(await)
            else
              F.start(evaluate) >>
                poll(await)

  /// SyncIO ///

  given UnsafeMemoizable[SyncIO] with
    extension[A](syncIO: SyncIO[A])
      def unsafeMemoize: SyncIO[A] =
        val lzy = Lazy.blocking:
          syncIO.run()
        SyncIO.delay(lzy.value)

  //extension [A](syncIO: SyncIO[A])
  //  def unsafeMemoize: SyncIO[A] =
  //    val lzy = Lazy.blocking:
  //      syncIO.run()
  //    SyncIO.delay(lzy.value)
