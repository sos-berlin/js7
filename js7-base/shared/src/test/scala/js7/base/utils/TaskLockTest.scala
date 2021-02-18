package js7.base.utils

import cats.effect.Resource
import js7.base.time.ScalaTime._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TaskLockTest extends AsyncFreeSpec
{
  private val n = 1000
  private val initial = 1

  "TaskLock" in {
    val lock = TaskLock("TEST", logWorryDurations = Nil)
    doTest(task => lock.lock(task))
      .map(o => assert(o == Vector.fill(n)(initial)))
      .runToFuture
  }

  "Other Resource does not lock" in {
    doTest(task => task)
      .map(o => assert(o != Vector.fill(n)(initial)))
      .runToFuture
  }

  private def doTest(body: Task[Int] => Task[Int]): Task[Seq[Int]] = {
    @volatile var guardedVariable = initial
    Task.parSequence(
      for (_ <- 1 to n) yield
        body {
          Task {
            val found = guardedVariable
            guardedVariable += 1
            found
          }.flatMap { found =>
            Task.sleep(100.µs) >>
              Task {
                guardedVariable = initial
                found
              }
          }
        })
  }
}
