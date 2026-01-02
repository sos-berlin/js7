package js7.base.utils

import cats.effect.{Resource, Sync}
import cats.kernel.Monoid
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import js7.base.Problems.ConcurrentAccessProblem
import js7.base.problem.Checked
import scala.annotation.targetName

type Atomic[A] = A match
  case Boolean => AtomicBoolean
  case Int => AtomicInteger
  case Long => AtomicLong
  case AnyRef => AtomicReference[A]


object Atomic:
  inline def apply(initial: Boolean): AtomicBoolean =
    new AtomicBoolean(initial)

  inline def apply(initial: Int): AtomicInteger =
    new AtomicInteger(initial)

  inline def apply(initial: Long): AtomicLong =
    new AtomicLong(initial)

  inline def apply[A <: AnyRef | Null](initial: A): AtomicReference[A] =
    new AtomicReference(initial)

  //transparent inline def apply(initial: Any): Any =
  //  initial match
  //    case o: Boolean => new AtomicBoolean(o)
  //    case o: Int => new AtomicInteger(o)
  //    case o: Long => new AtomicLong(o)
  //    case _: AnyRef => new AtomicReference(initial)

  object extensions:
    extension (atomic: AtomicBoolean)
      inline def :=(a: Boolean): Unit =
        atomic.set(a)

      def whenInUse[F[_] : Sync as F, A](whenInUse: => F[A]): WhenInUse[F, A] =
        WhenInUse[F, A](atomic, whenInUse)

      /** Assures non-concurrent access to the `body`.
        *
        * @return `Left(ConcurrentAccessProblem(what))` if `this` `AtomicBoolean` is `true`,
        *         otherwise the `body`.
        */
      def nonConcurrent[F[_] : Sync as F, A](what: String)(body: F[Checked[A]]): F[Checked[A]] =
        whenInUse(
          F.pure(Left(ConcurrentAccessProblem(what))),
          body)

      /** Assures non-concurrent access to the `body`.
        *
        * @return `A.empty` if `this` `AtomicBoolean` is `true`, otherwise the `body`.
        */
      def nonConcurrent[F[_] : Sync as F, A: Monoid as A](body: F[A]): F[A] =
        whenInUse(F.pure(A.empty), body)

      private[Atomic] def whenInUse[F[_] : Sync as F, A](whenInUse: => F[A], use: => F[A]): F[A] =
        Resource:
          F.delay:
            atomic.getAndSet(true) ->
              F.delay(atomic := false)
        .use:
          if _ then
            whenInUse
          else
            use

    extension (atomic: AtomicInteger)
      inline def :=(n: Int): Unit =
        atomic.set(n)

      inline def +=(n: Int): Unit =
        atomic.getAndAdd(n)

      @targetName("getAndIncrement")
      inline def +=(n: 1): Unit =
        atomic.getAndIncrement()

      inline def -=(n: Int): Unit =
        atomic.getAndAdd(-n)

      @targetName("getAndDecrement")
      inline def -=(n: 1): Unit =
        atomic.getAndDecrement()

      /** Count current Resource usage. */
      def countConcurrency[F[_]: Sync as F]: Resource[F, Unit] =
        Resource.make(
          acquire = F.delay(atomic += 1))(
          release = _ => F.delay(atomic -= 1))


    extension (atomic: AtomicLong)
      inline def :=(a: Long): Unit =
        atomic.set(a)

      @targetName("getAndIncrement")
      inline def +=(n: 1): Unit =
        atomic.getAndIncrement()

      @targetName("getAndIncrement")
      inline def +=(n: 1L): Unit =
        atomic.getAndIncrement()

      inline def +=(n: Long): Unit =
        atomic.getAndAdd(n)

      @targetName("getAndDecrement")
      inline def -=(n: 1): Unit =
        atomic.getAndDecrement()

      @targetName("getAndDecrement")
      inline def -=(n: 1L): Unit =
        atomic.getAndDecrement()

      inline def -=(n: Long): Unit =
        atomic.getAndAdd(-n)

      /** Count current Resource usage. */
      def gauge[F[_]: Sync as F]: Resource[F, Unit] =
        Resource.make(
          acquire = F.delay(atomic += 1))(
          release = _ => F.delay(atomic -= 1))


    extension[A](atomic: AtomicReference[A])
      inline def :=(a: A): Unit =
        atomic.set(a)


    final class WhenInUse[F[_] : Sync as F, A] private[Atomic](
      atomic: AtomicBoolean,
      whenInUse: => F[A]
    ):
      inline def otherwiseUse(inline use: F[A]): F[A] =
        atomic.whenInUse(whenInUse, use)
