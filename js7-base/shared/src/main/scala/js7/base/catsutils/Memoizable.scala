package js7.base.catsutils

import cats.FlatMap
import cats.effect.Concurrent
import cats.effect.concurrent.Deferred
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import java.util.concurrent.atomic.AtomicBoolean
import monix.eval.Task

/** Monix Task like (unsafe) memoize for Cats IO. */
trait Memoizable[F[_]]
{
  def memoize[A](f: F[A]): F[A]
}

object Memoizable
{
  def apply[F[_]](implicit o: Memoizable[F]) = o

  implicit val taskMemoizable: Memoizable[Task] =
    new Memoizable[Task] {
      def memoize[A](task: Task[A]) = task.memoize
    }

  implicit def concurrentMemoizable[F[_]: Concurrent: FlatMap]: Memoizable[F] =
    new Memoizable[F] {
      def memoize[A](io: F[A]): F[A] = {
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
    implicit class MemoizableIO[F[_], A](private val f: F[A]) extends AnyVal {
      def memoize(implicit m: Memoizable[F]): F[A] =
        m.memoize(f)
    }
  }
}
