package js7.base.utils

import js7.base.time.Stopwatch
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AsyncFreeSpec
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class AsyncLockTest extends AsyncFreeSpec
{
  private val initial = 1

  "With logging" - {
    addTests(n = 100, suppressLog = false)
  }

  "Without logging" - {
    addTests(n = 10_000, suppressLog = false)
  }

  def addTests(n: Int, suppressLog: Boolean): Unit = {
    "AsyncLock, concurrent" in {
      val lock = AsyncLock("TEST", logWorryDurations = Nil, suppressLog = suppressLog)
      doTest(lock.lock(_))
        .map(o => assert(o == Vector.fill(n)(initial)))
        .runToFuture
    }

    "No AsyncLock, concurrent" in {
      doTest(identity)
        .map(o => assert(o != Vector.fill(n)(initial)))
        .runToFuture
    }

    "AsyncLock, not concurrent" in {
      val lock = AsyncLock("TEST", logWorryDurations = Nil, suppressLog = suppressLog)
      Observable.fromIterable(1 to n)
        .map(_ => lock.lock(Task.unit))
        .completedL
        .timed.map { case (duration, ()) =>
          scribe.info(Stopwatch.itemsPerSecondString(duration, n))
          succeed
        }
        .runToFuture
    }

    def doTest(body: Task[Int] => Task[Int]): Task[Seq[Int]] = {
      @volatile var guardedVariable = initial
      Task.parSequence(
        for (_ <- 1 to n) yield
          body {
            Task {
              val found = guardedVariable
              idleNanos(10_000)
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

  private def idleNanos(nanos: Int): Unit = {
    val t = System.nanoTime()
    while (System.nanoTime() - t < nanos) {}
  }
}
