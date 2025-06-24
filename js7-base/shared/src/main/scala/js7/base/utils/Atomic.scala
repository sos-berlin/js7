package js7.base.utils

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
    extension(o: AtomicBoolean)
      inline def :=(a: Boolean): Unit =
        o.set(a)

    extension(o: AtomicInteger)
      inline def :=(a: Int): Unit =
        o.set(a)

      inline def +=(a: Int): Unit =
        o.getAndAdd(a)

      inline def -=(a: Int): Unit =
        o.getAndAdd(-a)

    extension(o: AtomicLong)
      inline def :=(a: Long): Unit =
        o.set(a)

      inline def +=(a: Long): Unit =
        o.getAndAdd(a)

      inline def -=(a: Long): Unit =
        o.getAndAdd(-a)

    extension[A](o: AtomicReference[A])
      inline def :=(a: A): Unit =
        o.set(a)
