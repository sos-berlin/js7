package js7.base.catsutils

import cats.effect.{Deferred, FiberIO, IO, Sync, SyncIO}
import cats.syntax.all.*
import js7.base.catsutils.UnsafeMemoizable.{memoize, given}
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import org.scalatest.Assertion
import scala.concurrent.CancellationException

final class UnsafeMemoizableTest extends OurAsyncTestSuite:

  "without .unsafeMemoize" in:
    check[IO](identity, 3)

  ".unsafeMemoize with Cats IO" in:
    checkIO(_.unsafeMemoize, 1)

  ".memoize.flatten with Cats IO does not memoize" in:
    checkIO(_.memoize.flatten, 3)

  ".unsafeMemoize with Cats SyncIO (blocking, experimental only)" in:
    check[SyncIO](_.unsafeMemoize, 1).unsafeRunSync()

  "cancel" in:
    // The first caller can be canceled. The evaluation is not canceled,
    // it continues in background and can be used by a later call.
    val called = Atomic(0)
    val started = Deferred.unsafe[IO, Unit]
    val m = memoize:
      IO(called.incrementAndGet()) *>
        started.complete(()) *>
        IO.sleep(50.ms).as(3)
    for
      fiber <- m.start
      _ <- started.get
      _ <- fiber.cancel
      outcome <- fiber.join
      _ = assert(outcome.isCanceled)
      x <- m
      _ = assert(x == 3 && called.get == 1)
    yield
      succeed

  "Callers can be canceled while the evaluation never completes" in:
    val m = memoize(IO.never[Int])
    for
      fiber1 <- m.start
      _ <- IO.sleep(10.ms)
      _ <- cancelWithin9s(fiber1) // would block, if the first caller were uncancelable
      fiber2 <- m.start
      _ <- IO.sleep(10.ms)
      _ <- cancelWithin9s(fiber2)
      outcome1 <- fiber1.join
      outcome2 <- fiber2.join
    yield
      assert(outcome1.isCanceled && outcome2.isCanceled)

  "Parallel callers share one evaluation" in:
    val called = Atomic(0)
    val m = memoize:
      IO.sleep(50.ms) *> IO(called.incrementAndGet())
    for
      results <- List.fill(20)(m).parSequence
    yield
      assert(results.forall(_ == 1) && called.get == 1)

  "A failure is memoized, too" in:
    val called = Atomic(0)
    val m = memoize:
      IO(called.incrementAndGet()) *> IO.raiseError[Int](new RuntimeException("TEST"))
    for
      a <- m.attempt
      b <- m.attempt
    yield
      assert(a.left.exists(_.getMessage == "TEST") && b.left.exists(_.getMessage == "TEST")
        && called.get == 1)

  "An evaluation canceling itself does not let callers wait forever" in:
    val m = memoize(IO.canceled *> IO.pure(1))
    for
      a <- m.attempt.timeout(9.s)
      b <- m.attempt.timeout(9.s)
    yield
      assert(a.left.exists(_.isInstanceOf[CancellationException])
        && b.left.exists(_.isInstanceOf[CancellationException]))

  /** `fiber.cancel` is uncancelable itself, so it must run in a fiber of its own
    * to let a timeout fail the test instead of hanging it. */
  private def cancelWithin9s(fiber: FiberIO[?]): IO[Unit] =
    fiber.cancel.start.flatMap(_.join).timeout(9.s).void

  private def check[F[_]: Sync](f: F[Int] => F[Int], expected: Int): F[Assertion] =
    val called = Atomic(0)
    // TODO How to test parallel execution?
    val x: F[Int] = f(Sync[F].delay(called.incrementAndGet()))
    val fail: F[Int] = f(Sync[F].raiseError(new RuntimeException("TEST")))
    for
      _ <- fail.attempt
      _ <- x
      _ <- x
      a <- x
    yield assert(a == expected)

  private def checkIO(f: IO[Int] => IO[Int], expected: Int): IO[Assertion] =
    val called = Atomic(0)
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
