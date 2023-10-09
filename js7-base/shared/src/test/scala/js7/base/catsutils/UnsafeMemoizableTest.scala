package js7.base.catsutils

import cats.effect.{Concurrent, ContextShift, IO, Sync, SyncIO, Timer}
import cats.syntax.all.*
import cats.{FlatMap, effect}
import java.util.concurrent.atomic.AtomicInteger
import js7.base.catsutils.UnsafeMemoizable.given
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import monix.eval.Task
import org.scalatest.Assertion
import scala.concurrent.ExecutionContext

final class UnsafeMemoizableTest extends OurAsyncTestSuite:

  "without unsafeMemoize" in:
    given ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    check[IO](identity, 3).unsafeToFuture()

  "unsafeMemoize with Cats IO" in:
    given ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    given Timer[IO] = IO.timer(ExecutionContext.global)
    checkIO(_.unsafeMemoize, 1).unsafeToFuture()

  "unsafeMemoize with Cats SyncIO (experimental only)" in:
    check[SyncIO](_.unsafeMemoize, 1).unsafeRunSync()

  "unsafeMemoize with Monix Task, direct" in:
    import monix.execution.Scheduler.Implicits.traced
    check[Task](_.unsafeMemoize, 1).runToFuture

  "Original memoize with Monix Task, direct" in:
    import monix.execution.Scheduler.Implicits.traced
    check[Task](_.memoize/*native*/, 1).runToFuture

  "unsafeMemoize with Monix Task, via Concurrent" in:
    import monix.execution.Scheduler.Implicits.traced
    check0[Task](1).runToFuture

  private def check0[F[_]: Concurrent: ContextShift: FlatMap](expected: Int): F[Assertion] =
    // These two calls are equivalent
    for
      _ <- check[F](_.unsafeMemoize, expected)
      _ <- check[F](UnsafeMemoizable.concurrentMemoizable.unsafeMemoize(_), expected)
    yield succeed

  private def check[F[_]: Sync: FlatMap](f: F[Int] => F[Int], expected: Int): F[Assertion] =
    val called = new AtomicInteger(0)
    // TODO How to test parallel execution?
    val x: F[Int] = f(Sync[F].delay(called.incrementAndGet()))
    val fail: F[Int] = f(Sync[F].raiseError(new RuntimeException("TEST")))
    for
      _ <- fail.attempt
      _ <- x
      _ <- x
      a <- x
    yield assert(a == expected)

  private def checkIO(f: IO[Int] => IO[Int], expected: Int)(using ContextShift[IO], Timer[IO])
  : IO[Assertion] =
    val called = new AtomicInteger(0)
    // TODO How to test parallel execution?
    val increment: IO[Int] = f(Sync[IO].delay(called.incrementAndGet()))
    val fail: IO[Int] = f(Sync[IO].raiseError(new RuntimeException("TEST")))
    // TODO How to test a cancelled operation?
    //val cancel: IO[Int] = ?
    for
      _ <- fail.attempt
      //_ <- cancel
      //_ <- cancel
      _ <- IO(Logger[this.type].info("cancel fertig"))
      _ <- increment
      _ <- increment
      a <- increment
    yield assert(a == expected)
