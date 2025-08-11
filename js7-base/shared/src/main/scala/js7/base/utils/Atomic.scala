package js7.base.utils

import cats.effect.Sync
import cats.effect.kernel.Resource
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
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
    extension(atomic: AtomicBoolean)
      inline def :=(a: Boolean): Unit =
        atomic.set(a)


    extension(atomic: AtomicInteger)
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


    extension(atomic: AtomicLong)
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
