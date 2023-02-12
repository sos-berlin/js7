package js7.base.catsutils

import cats.FlatMap
import cats.effect.Concurrent
import cats.effect.concurrent.Deferred
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import java.util.concurrent.atomic.AtomicBoolean
import monix.eval.Task

/** Monix Task like (unsafe) memoize for Cats IO. */
trait UnsafeMemoizable[F[_]]
{
  def unsafeMemoize[A](f: F[A]): F[A]
}

object UnsafeMemoizable
{
  def apply[F[_]](implicit o: UnsafeMemoizable[F]) = o

  implicit val taskMemoizable: UnsafeMemoizable[Task] =
    new UnsafeMemoizable[Task] {
      def unsafeMemoize[A](task: Task[A]) = task.memoize
    }

  implicit def concurrentMemoizable[F[_]: Concurrent: FlatMap]: UnsafeMemoizable[F] =
    new UnsafeMemoizable[F] {
      def unsafeMemoize[A](io: F[A]): F[A] = {
        val triggered = new AtomicBoolean(false)
        val deferred = Deferred.unsafe[F, A]

        Concurrent[F].defer {
          if (!triggered.getAndSet(true))
            io.flatMap(a => deferred.complete(a).as(a))
          else
            deferred.get
        }
      }
    }

  object syntax {
    implicit class RichMemoizable[F[_], A](private val f: F[A]) extends AnyVal {
      def unsafeMemoize(implicit m: UnsafeMemoizable[F]): F[A] =
        m.unsafeMemoize(f)
    }
  }
}
