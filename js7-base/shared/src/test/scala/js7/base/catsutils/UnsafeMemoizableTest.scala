package js7.base.catsutils

import cats.effect.{IO, Sync, SyncIO}
import cats.syntax.all.*
import cats.{FlatMap, effect}
import java.util.concurrent.atomic.AtomicInteger
import js7.base.catsutils.UnsafeMemoizable.given
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Atomic
import org.scalatest.Assertion

final class UnsafeMemoizableTest extends OurAsyncTestSuite:

  "without .unsafeMemoize" in:
    check[IO](identity, 3)

  ".unsafeMemoize with Cats IO" in:
    checkIO(_.unsafeMemoize, 1)

  ".memoize.flatten with Cats IO does not memoize" in:
    checkIO(_.memoize.flatten, 3)

  ".unsafeMemoize with Cats SyncIO (blocking, experimental only)" in:
    check[SyncIO](_.unsafeMemoize, 1).unsafeRunSync()

  private def check[F[_]: Sync: FlatMap](f: F[Int] => F[Int], expected: Int): F[Assertion] =
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
