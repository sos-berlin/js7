package js7.base.monixutils

import cats.syntax.apply.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.ByteUnits
import monix.eval.{Task, TaskLocal}
import monix.execution.Scheduler.Implicits.traced
import monix.execution.misc.Local
import monix.reactive.Observable

/** Some tests to show how Monix works. */
final class MonixTests extends OurAsyncTestSuite
{
  "Local" in {
    val local = Local("")
    val task = for
      _ <- Task(local := "1")
      _ <- Task(assert(local.get == "1"))
      _ <- Task.shift
      _ <- Task(assert(local.get == "1"))
      _ <- TaskLocal.isolate(Task(local := "1"))
      _ <- Task(assert(local.get == "1"))
    yield succeed
    task.runToFuture
  }

  "TaskLocal #2" in {
    val task = for
      local <- TaskLocal("")
      _ <- local.write("1")
      v <- local.read
      _ <- Task(assert(v == "1"))
      _ <- Task.shift
      v <- local.read
      _ <- Task(assert(v == "1"))
      _ <- TaskLocal.isolate(local.write("2"))
      v <- local.read
      _ <- Task(assert(v == "1"))
    yield succeed
    task.runToFuture
  }

  "TaskLocal" in {
    val local = TaskLocal("").memoize
    val task = for
      _ <- local.flatMap(_.write("1"))
      v <- local.flatMap(_.read)
      _ <- Task(assert(v == "1"))
      _ <- Task.shift
      v <- local.flatMap(_.read)
      _ <- Task(assert(v == "1"))
      _ <- TaskLocal.isolate(local.flatMap(_.write("2")))
      v <- local.flatMap(_.read)
      _ <- Task(assert(v == "1"))
    yield succeed
    task.runToFuture
  }

  "Observable.tailRecM with possible OutOfMemoryError" - {
    // Observable.tailRecM may leak memory !!!  https://github.com/monix/monix/issues/791

    if false then "not leaking" in {
      check(i =>
        Observable(Right(i), Left(i + 1)))
    }

    if false then "leaks" in {
      // Fails with Observable.empty after Left !!!
      check(i =>
        Observable(Right(i), Left(i + 1)) ++ Observable.empty)
    }

    if false then "fails with OutOfMemoryError" in {
      // Fails !!!
      check(i =>
        Observable.pure(Right(i)) ++
          Observable.fromTask(Task.sleep(1.ns)) *>
            Observable.pure(Left(i + 1)))
    }

    if false then "not leaking" in {
      check(i =>
        Observable.pure(Right(i)) ++
          Observable.evalDelayed(1.ns, Left(i + 1)))
    }

    if false then "not leaking" in {
      check(i =>
        Observable.pure(Right(i)) ++
          Observable.pure(Left(i + 1)).delayExecution(1.ns))
    }

    def check(obs: Long => Observable[Either[Long, Long]]) =
      Observable
        .tailRecM(0L)(obs)
        .map(i => if i % 1000_000 == 0 then
          println(s"$i ${ByteUnits.toKBGB(sys.runtime.totalMemory())}"))
        .completedL
        .map(_ => fail())
        .runToFuture
  }
}
