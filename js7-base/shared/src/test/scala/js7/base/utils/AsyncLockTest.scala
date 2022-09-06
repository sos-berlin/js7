package js7.base.utils

import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.Promise
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class AsyncLockTest extends AsyncFreeSpec
{
  private val initial = 1

  "With logging" - {
    addTests(n = 200, suppressLog = false)
  }

  "Without logging" - {
    addTests(n = 10_000, suppressLog = false)
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
      if (sys.runtime.availableProcessors == 1) {
        fail("This concurrent test requires more than one processor")
      } else {
        doTest(identity)
          // May occasional be equal despite lock is misssing !!!
          .map(o => assert(o != Vector.fill(n)(initial)))
          .runToFuture
      }
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
      val guardedVariable = Atomic(initial)
      val idleDuration = 100.µs
      Task.parSequence(
        for (_ <- 1 to n) yield
          body {
            Task {
              val found = guardedVariable.get()
              idleNanos(idleDuration)
              guardedVariable += 1
              found
            } .tapEval(_ => if (Random.nextBoolean()) Task.shift else Task.unit)
              .flatMap { found =>
                Task {
                  guardedVariable := initial
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

  "Cancel releases lock only after task has been canceled" in {
    val lock = AsyncLock("CANCEL", logWorryDurations = Nil)
    val started = Promise[Unit]()
    val lockCanceled = Promise[Unit]()
    val taskCanceled = Promise[Unit]()
    val continue = Promise[Unit]()
    val taskCompleted = Promise[Unit]()

    val future = lock
      .lock(Task.defer {
        started.success(())
        Task.sleep(99.s)
          .doOnCancel(
            Task.sleep(500.ms)
              .*>(Task.defer {
                assert(!lockCanceled.isCompleted)
                taskCanceled.success(())
                  Task.fromFuture(continue.future)
                    .*>(Task {
                      assert(!lockCanceled.isCompleted)
                      taskCompleted.success(())
                    })
              }))
      })
      .runToFuture

    Task
      .fromFuture(started.future)
      .flatMap(_ => Task.defer {
        future.cancel()

        Task.parMap2(
          lock.lock(Task.unit)
            .*>(Task {
              lockCanceled.success(())
              assert(taskCompleted.isCompleted)
            })
            .timeoutWith(9.s, new RuntimeException("Cancel operation has not released the lock")),

          Task.fromFuture(taskCanceled.future)
            .*>(Task {
              assert(!lockCanceled.isCompleted)
              assert(!taskCompleted.isCompleted)
            })
            .*>(
              Task {
                continue.success(())
              }.delayExecution(500.ms))
        )((_, _) => ())
          .*>(
            Task
              .fromFuture(taskCanceled.future)
              .timeoutWith(9.s, new RuntimeException("Task has not been canceled"))
          )
          .as(succeed)
      })
      .runToFuture
  }

  private def idleNanos(duration: FiniteDuration): Unit = {
    val t = System.nanoTime()
    val nanos = duration.toNanos
    while (System.nanoTime() - t < nanos) {}
  }
}
