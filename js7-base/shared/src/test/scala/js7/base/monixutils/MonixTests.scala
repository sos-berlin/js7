package js7.base.monixutils

import monix.eval.{Task, TaskLocal}
import monix.execution.Scheduler.Implicits.traced
import monix.execution.misc.Local
import org.scalatest.freespec.AsyncFreeSpec

/** Some tests to show how Monix works. */
final class MonixTests extends AsyncFreeSpec
{
  "Local" in {
    val local = Local("")
    val task = for {
      _ <- Task(local := "1")
      _ <- Task(assert(local.get == "1"))
      _ <- Task.shift
      _ <- Task(assert(local.get == "1"))
      _ <- TaskLocal.isolate(Task(local := "1"))
      _ <- Task(assert(local.get == "1"))
    } yield succeed
    task.runToFuture
  }

  "TaskLocal #2" in {
    val task = for {
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
    } yield succeed
    task.runToFuture
  }

  "TaskLocal" in {
    val local = TaskLocal("").memoize
    val task = for {
      _ <- local.flatMap(_.write("1"))
      v <- local.flatMap(_.read)
      _ <- Task(assert(v == "1"))
      _ <- Task.shift
      v <- local.flatMap(_.read)
      _ <- Task(assert(v == "1"))
      _ <- TaskLocal.isolate(local.flatMap(_.write("2")))
      v <- local.flatMap(_.read)
      _ <- Task(assert(v == "1"))
    } yield succeed
    task.runToFuture
  }
}
