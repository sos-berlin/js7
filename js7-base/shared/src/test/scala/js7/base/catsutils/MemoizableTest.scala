package js7.base.catsutils

import cats.effect.IO
import java.util.concurrent.atomic.AtomicInteger
import js7.base.catsutils.Memoizable.syntax.*
import js7.base.test.OurAsyncTestSuite
import org.scalatest.Assertion
import scala.concurrent.{ExecutionContext, Future}

final class MemoizableTest extends OurAsyncTestSuite
{
  "without memoize" in {
    check(identity, 3)
  }

  "memoize" in {
    implicit val cs = IO.contextShift(ExecutionContext.global)
    check(_.memoize, 1)
  }

  private def check(f: IO[Int] => IO[Int], expected: Int): Future[Assertion] = {
    val called = new AtomicInteger(0)
    val io = f(IO(called.incrementAndGet()))
    (for {
      _ <- io
      _ <- io
      a <- io
    } yield assert(a == expected))
      .unsafeToFuture()
  }
}
