package js7.base.catsutils

import cats.FlatMap
import cats.effect.{Concurrent, IO, Sync}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import java.util.concurrent.atomic.AtomicInteger
import js7.base.catsutils.Memoizable.syntax.*
import js7.base.test.OurAsyncTestSuite
import monix.eval.Task
import org.scalatest.Assertion
import scala.concurrent.ExecutionContext

final class MemoizableTest extends OurAsyncTestSuite
{
  "without memoize" in {
    implicit val concurrent: Concurrent[IO] = makeConcurrent()
    check[IO](identity, 3).unsafeToFuture()
  }

  "memoize with Cats IO" in {
    implicit val concurrent: Concurrent[IO] = makeConcurrent()
    check[IO](_.memoize, 1).unsafeToFuture()
  }

  def makeConcurrent(): Concurrent[IO] =  {
    implicit val cs = IO.contextShift(ExecutionContext.global)
    implicitly[Concurrent[IO]]
  }

  "memoize with Monix Task, direct" in {
    import monix.execution.Scheduler.Implicits.traced
    check[Task](_.memoize, 1).runSyncUnsafe()
  }

  "memoize with Monix Task, via Concurrent" in {
    import monix.execution.Scheduler.Implicits.traced
    check0[Task](1).runSyncUnsafe()
  }

  private def check0[F[_]: Concurrent: FlatMap](expected: Int): F[Assertion] = {
    // These two calls are equivalent
    check[F](_.memoize, expected)
    check[F](Memoizable.concurrentMemoizable.memoize(_), expected)
  }

  private def check[F[_]: Sync: FlatMap](f: F[Int] => F[Int], expected: Int): F[Assertion] = {
    val called = new AtomicInteger(0)
    val io = f(Sync[F].delay(called.incrementAndGet()))
    (for {
      _ <- io
      _ <- io
      a <- io
    } yield assert(a == expected))
  }
}
