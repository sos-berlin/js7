package js7.base.utils

import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.utils.AsyncLockTest.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import scala.concurrent.Promise
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class AsyncLockTest extends OurAsyncTestSuite
{
  private val initial = 1
  private val n = 1000

  "With logging" - {
    addTests(n, suppressLog = false)
  }

  "Without logging" - {
    addTests(n, suppressLog = true)
  }

  def addTests(n: Int, suppressLog: Boolean): Unit = {
    "AsyncLock, concurrent" in {
      val lock = AsyncLock("TEST", logWorryDurations = Nil, suppressLog = suppressLog)
      doTest(lock.lock(_))
        .map(o => assert(o == Vector.fill(n)(initial)))
        // High probability to fail once
        .onErrorRestartLoop(100_000) {
          case (t, 0, _) => Task.raiseError(t)
          case (_, i, retry) => Task.sleep(1.ms) *> retry(i - 1)
        }
        .runToFuture
    }

    "No AsyncLock, concurrent" in {
      if sys.runtime.availableProcessors == 1 then {
        fail("This concurrent test requires more than one processor")
      } else {
        val maxTries = 100
        val expected = Vector.fill(n)(initial)
        Task
          .tailRecM(0)(i =>
            doTest(identity).flatMap(result =>
              if i < maxTries && result == expected then {
                logger.warn("Retry because tasks did not run concurrently")
                Task.left(i + 1)
              } else
                Task.right(assert(result != expected))))
          .runToFuture
      }
    }

    "AsyncLock, not concurrent" in {
      val lock = AsyncLock("TEST", logWorryDurations = Nil, suppressLog = suppressLog)
      Observable.fromIterable(1 to n)
        .mapEval(_ => lock.lock(Task.unit))
        .completedL
        .timed.map { case (duration, ()) =>
          logger.info(Stopwatch.itemsPerSecondString(duration, n))
          succeed
        }
        .runToFuture
    }

    def doTest(body: Task[Int] => Task[Int]): Task[Seq[Int]] = {
      val guardedVariable = Atomic(initial)
      val idleDuration = 100.µs
      Task.parSequence(
        for _ <- 1 to n yield
          body {
            Task {
              val found = guardedVariable.get()
              idleNanos(idleDuration)
              guardedVariable += 1
              found
            } .tapEval(_ => if Random.nextBoolean() then Task.shift else Task.unit)
              .flatMap { found =>
                Task {
                  guardedVariable := initial
                  found
                }
              }
          })
        .timed.map { case (duration, result) =>
          logger.info(Stopwatch.itemsPerSecondString(duration, n))
          result
      }
    }

    def idleNanos(duration: FiniteDuration): Unit = {
      val t = System.nanoTime()
      val nanos = duration.toNanos
      while System.nanoTime() - t < nanos do {}
    }
  }

  "Cancel releases lock only after task has been canceled" in {
    val lock = AsyncLock("CANCEL", logWorryDurations = Nil)
    val taskStarted = Promise[Unit]()
    val taskCancelationStarted = Promise[Unit]()
    val taskCompleted = Promise[Unit]()
    val continue = Promise[Unit]()

    val future = lock
      .lock(Task.defer {
        taskStarted.success(())
        Task.never
          .doOnCancel(Task.defer {
            taskCancelationStarted.success(())
            Task.sleep(100.ms)
              .*>(Task.defer {
                Task.fromFuture(continue.future)
                  .*>(Task {
                    taskCompleted.success(())
                  })
              })
          })
      })
      .runToFuture

    Task
      .fromFuture(taskStarted.future)
      .flatMap(_ => Task.defer {
        future.cancel()

        Task
          .fromFuture(taskCancelationStarted.future)
          .*>(Task {
            assert(!taskCompleted.isCompleted)
            continue.success(())
          })
          .*>(lock
            .lock(
              Task.fromFuture(taskCompleted.future))
            .timeoutWith(9.s, new RuntimeException("Cancel operation has not released the lock")))
          .as(succeed)
      })
      .runToFuture
  }
}

object AsyncLockTest
{
  private val logger = js7.base.log.Logger[this.type]
}
