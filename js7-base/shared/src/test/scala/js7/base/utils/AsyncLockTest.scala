package js7.base.utils

import js7.base.time.Stopwatch
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class AsyncLockTest extends AsyncFreeSpec
{
  private val n = 10000
  private val initial = 1

  "AsyncLock" in {
    val lock = AsyncLock("TEST", logWorryDurations = Nil)
    doTest(lock.lock(_))
      .map(o => assert(o == Vector.fill(n)(initial)))
      .runToFuture
  }

  "Same test without lock" in {
    doTest(identity)
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
          } .tapEval(_ => if (Random.nextBoolean()) Task.shift else Task.unit)
            .flatMap { found =>
              Task {
                guardedVariable = initial
                found
              }
            }
        })
      .timed.map { case (duration, result) =>
        scribe.info(Stopwatch.itemsPerSecondString(duration, n))
        result
    }
  }
}
