package js7.base.monixutils

import cats.syntax.apply.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.ByteUnits
import cats.effect.IO
import cats.effect.Fiber
import monix.eval.{Task, TaskLocal}
import monix.execution.Scheduler.Implicits.traced
import monix.execution.misc.Local
import fs2.Stream

/** Some tests to show how Monix works. */
final class MonixTests extends OurAsyncTestSuite
{
  "Local" in {
    val local = Local("")
    val io = for
      _ <- IO(local := "1")
      _ <- IO(assert(local.get == "1"))
      _ <- IO.shift
      _ <- IO(assert(local.get == "1"))
      _ <- IOLocal.isolate(IO(local := "1"))
      _ <- IO(assert(local.get == "1"))
    yield succeed
    io.runToFuture
  }

  "IOLocal #2" in {
    val io = for
      local <- IOLocal("")
      _ <- local.write("1")
      v <- local.read
      _ <- IO(assert(v == "1"))
      _ <- IO.shift
      v <- local.read
      _ <- IO(assert(v == "1"))
      _ <- IOLocal.isolate(local.write("2"))
      v <- local.read
      _ <- IO(assert(v == "1"))
    yield succeed
    io.runToFuture
  }

  "IOLocal" in {
    val local = IOLocal("").memoize
    val io = for
      _ <- local.flatMap(_.write("1"))
      v <- local.flatMap(_.read)
      _ <- IO(assert(v == "1"))
      _ <- IO.shift
      v <- local.flatMap(_.read)
      _ <- IO(assert(v == "1"))
      _ <- IOLocal.isolate(local.flatMap(_.write("2")))
      v <- local.flatMap(_.read)
      _ <- IO(assert(v == "1"))
    yield succeed
    io.runToFuture
  }

  "Stream.tailRecM with possible OutOfMemoryError" - {
    // Stream.tailRecM may leak memory !!!  https://github.com/monix/monix/issues/791

    if false then "not leaking" in {
      check(i =>
        Stream(Right(i), Left(i + 1)))
    }

    if false then "leaks" in {
      // Fails with Stream.empty after Left !!!
      check(i =>
        Stream(Right(i), Left(i + 1)) ++ Stream.empty)
    }

    if false then "fails with OutOfMemoryError" in {
      // Fails !!!
      check(i =>
        Stream.emit(Right(i)) ++
          Stream.fromIO(IO.sleep(1.ns)) *>
            Stream.emit(Left(i + 1)))
    }

    if false then "not leaking" in {
      check(i =>
        Stream.emit(Right(i)) ++
          Stream.evalDelayed(1.ns, Left(i + 1)))
    }

    if false then "not leaking" in {
      check(i =>
        Stream.emit(Right(i)) ++
          Stream.emit(Left(i + 1)).delayExecution(1.ns))
    }

    def check(obs: Long => Stream[IO, Either[Long, Long]]) =
      Stream
        .tailRecM(0L)(obs)
        .map(i => if i % 1000_000 == 0 then
          println(s"$i ${ByteUnits.toKBGB(sys.runtime.totalMemory())}"))
        .completedL
        .map(_ => fail())
        .runToFuture
  }
}
