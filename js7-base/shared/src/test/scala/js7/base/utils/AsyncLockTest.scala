package js7.base.utils

import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.utils.AsyncLockTest.*
import js7.base.utils.Atomic.extensions.*
import cats.effect.IO
import monix.execution.Scheduler.Implicits.traced
import fs2.Stream
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
          case (t, 0, _) => IO.raiseError(t)
          case (_, i, retry) => IO.sleep(1.ms) *> retry(i - 1)
        }
        .runToFuture
    }

    "No AsyncLock, concurrent" in {
      if sys.runtime.availableProcessors == 1 then {
        fail("This concurrent test requires more than one processor")
      } else {
        val maxTries = 100
        val expected = Vector.fill(n)(initial)
        IO
          .tailRecM(0)(i =>
            doTest(identity).flatMap(result =>
              if i < maxTries && result == expected then {
                logger.warn("Retry because ios did not run concurrently")
                IO.left(i + 1)
              } else
                IO.right(assert(result != expected))))
          .runToFuture
      }
    }

    "AsyncLock, not concurrent" in {
      val lock = AsyncLock("TEST", logWorryDurations = Nil, suppressLog = suppressLog)
      Stream.fromIterable(1 to n)
        .mapEval(_ => lock.lock(IO.unit))
        .completedL
        .timed.map { case (duration, ()) =>
          logger.info(Stopwatch.itemsPerSecondString(duration, n))
          succeed
        }
        .runToFuture
    }

    def doTest(body: IO[Int] => IO[Int]): IO[Seq[Int]] = {
      val guardedVariable = Atomic(initial)
      val idleDuration = 100.µs
      IO.parSequence(
        for _ <- 1 to n yield
          body {
            IO {
              val found = guardedVariable.get()
              idleNanos(idleDuration)
              guardedVariable += 1
              found
            } .tapEval(_ => if Random.nextBoolean() then IO.shift else IO.unit)
              .flatMap { found =>
                IO {
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

  "Cancel releases lock only after io has been canceled" in {
    val lock = AsyncLock("CANCEL", logWorryDurations = Nil)
    val ioStarted = Promise[Unit]()
    val ioCancelationStarted = Promise[Unit]()
    val ioCompleted = Promise[Unit]()
    val continue = Promise[Unit]()

    val future = lock
      .lock(IO.defer {
        ioStarted.success(())
        IO.never
          .doOnCancel(IO.defer {
            ioCancelationStarted.success(())
            IO.sleep(100.ms)
              .*>(IO.defer {
                IO.fromFuture(continue.future)
                  .*>(IO {
                    ioCompleted.success(())
                  })
              })
          })
      })
      .runToFuture

    IO
      .fromFuture(ioStarted.future)
      .flatMap(_ => IO.defer {
        future.cancel()

        IO
          .fromFuture(ioCancelationStarted.future)
          .*>(IO {
            assert(!ioCompleted.isCompleted)
            continue.success(())
          })
          .*>(lock
            .lock(
              IO.fromFuture(ioCompleted.future))
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
