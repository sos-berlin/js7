package js7.base.utils

import cats.effect.Sync
import cats.effect.kernel.Resource
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}

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
    extension(atomic: AtomicBoolean)
      inline def :=(a: Boolean): Unit =
        atomic.set(a)


    extension(atomic: AtomicInteger)
      inline def :=(a: Int): Unit =
        atomic.set(a)

      inline def +=(a: Int): Unit =
        atomic.getAndAdd(a)

      inline def -=(a: Int): Unit =
        atomic.getAndAdd(-a)

      /** Count current Resource usage. */
      def gauge[F[_]: Sync as F]: Resource[F, Unit] =
        Resource.make(
          acquire = F.delay(atomic += 1))(
          release = _ => F.delay(atomic -= 1))


    extension(atomic: AtomicLong)
      inline def :=(a: Long): Unit =
        atomic.set(a)

      inline def +=(a: Long): Unit =
        atomic.getAndAdd(a)

      inline def -=(a: Long): Unit =
        atomic.getAndAdd(-a)

      /** Count current Resource usage. */
      def gauge[F[_]: Sync as F]: Resource[F, Unit] =
        Resource.make(
          acquire = F.delay(atomic += 1))(
          release = _ => F.delay(atomic -= 1))


    extension[A](atomic: AtomicReference[A])
      inline def :=(a: A): Unit =
        atomic.set(a)
