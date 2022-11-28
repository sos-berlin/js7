package js7.base.catsutils

import cats.effect.{ContextShift, IO}
import java.util.concurrent.atomic.AtomicBoolean
import monix.eval.Task
import scala.concurrent.Promise

/** Monix Task like (unsafe) memoize for Cats IO. */
trait Memoizable[F[_]]
{
  def memoize[A](f: F[A]): F[A]
}

object Memoizable
{
  def apply[F[_]](implicit o: Memoizable[F]) = o

  implicit def ioMemoizable(implicit cs: ContextShift[IO]): Memoizable[IO] =
    new Memoizable[IO]
    {
      def memoize[A](io: IO[A]): IO[A] = {
        val triggered = new AtomicBoolean(false)
        val p = Promise[A]()

        IO.defer {
          if (!triggered.getAndSet(true)) {
            p.completeWith(io.unsafeToFuture())
          }
          IO.fromFuture(IO.pure(p.future))
        }
      }
    }

  implicit val taskMemoizable: Memoizable[Task] =
    new Memoizable[Task]
    {
      def memoize[A](task: Task[A]) = task.memoize
    }

  implicit class MemoizableIO[F[_], A](private val f: F[A]) extends AnyVal {
    def memoize(implicit m: Memoizable[F]): F[A] =
      m.memoize(f)
  }
}
