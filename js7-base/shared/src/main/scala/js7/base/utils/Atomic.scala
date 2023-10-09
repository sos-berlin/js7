package js7.base.utils

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}

object Atomic:
  inline def apply(initial: Boolean): AtomicBoolean =
    new AtomicBoolean(initial)

  inline def apply(initial: Int): AtomicInteger =
    new AtomicInteger(initial)

  inline def apply(initial: Long): AtomicLong =
    new AtomicLong(initial)

  inline def apply[A <: AnyRef](initial: A): AtomicReference[A] =
    new AtomicReference(initial)

  //inline def apply(initial: Any): Any =
  //  inline initial match
  //    case o: Boolean => new AtomicBoolean(o)
  //    case o: Int => new AtomicInteger(o)
  //    case o if o.isInstanceOf[AnyRef] => new AtomicReference(o)

  object syntax:
    extension(o: AtomicBoolean)
      def :=(a: Boolean): Unit =
        o.set(a)

    extension(o: AtomicInteger)
      def :=(a: Int): Unit =
        o.set(a)

      def +=(a: Int): Int =
        o.addAndGet(a)

      def -=(a: Int): Int =
        o.addAndGet(-a)

    extension(o: AtomicLong)
      def :=(a: Long): Unit =
        o.set(a)

      def +=(a: Long): Long =
        o.addAndGet(a)

      def -=(a: Long): Long =
      o.addAndGet(-a)

    extension[A](o: AtomicReference[A])
      def :=(a: A): Unit =
        o.set(a)
