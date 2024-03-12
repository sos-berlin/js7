package js7.base.utils

import cats.effect.{Deferred, IO}
import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import fs2.Stream
import js7.base.catsutils.CatsEffectExtensions.{left, right}
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.utils.AsyncLockTest.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.Tests.isIntelliJIdea
import org.scalatest.{Assertion, Assertions}
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class AsyncLockTest extends OurAsyncTestSuite:

  private val initial = 1
  private val n = 1000

  "With logging" - {
    addTests(n, suppressLog = false)
  }

  "Without logging" - {
    addTests(n, suppressLog = true)
  }

  private def addTests(n: Int, suppressLog: Boolean): Unit = {
    "AsyncLock, concurrent" in {
      var retryCount = 0
      val lock = AsyncLock("TEST", logWorryDurations = Nil, suppressLog = suppressLog)
      doTest(lock.lock(_))
        .map(o => assert(o == Vector.fill(n)(initial)))
        // High probability to fail once
        .onErrorRestartLoop(100_000) {
          case (t, 0, _) => IO.raiseError(t)
          case (_, i, retry) =>
            retryCount += 1
            IO.sleep(1.ms) *> retry(i - 1)
        }
        .<*(IO(logger.info(s"$retryCount retries")))
    }

    "No AsyncLock, concurrent" in {
      if sys.runtime.availableProcessors == 1 then {
        fail("This concurrent test requires more than one processor")
      } else {
        val maxTries = 100
        val expected = Vector.fill(n)(initial)
        0.tailRecM(i =>
          doTest(identity).flatMap(result =>
            if i < maxTries && result == expected then {
              logger.warn("Retry because IOs did not run concurrently")
              IO.left(i + 1)
            } else
              IO.right(assert(result != expected))))
      }
    }

    "AsyncLock, not concurrent" in {
      val lock = AsyncLock("TEST", logWorryDurations = Nil, suppressLog = suppressLog)
      Stream.emits(1 to n)
        .evalMap(_ => lock.lock(IO.unit))
        .compile
        .drain
        .timed.map { case (duration, ()) =>
          logger.info(Stopwatch.itemsPerSecondString(duration, n))
          succeed
        }
    }

    def doTest(body: IO[Int] => IO[Int]): IO[Seq[Int]] =
      val guardedVariable = Atomic(initial)
      val idleDuration = 100.µs
      val ios: Seq[IO[Int]] =
        for i <- 1 to n yield
          body:
            IO:
              val found = guardedVariable.get()
              idleNanos(idleDuration)
              guardedVariable += 1
              found
            .flatTap(_ => IO.whenA(Random.nextBoolean()):
              IO.cede)
            .flatMap: found =>
              IO:
                guardedVariable := initial
                found
      ios.parSequence
        .timed.flatMap: (duration, result) =>
          IO:
            logger.info(Stopwatch.itemsPerSecondString(duration, n))
            result

    def idleNanos(duration: FiniteDuration): Unit = {
      val t = System.nanoTime()
      val nanos = duration.toNanos
      while System.nanoTime() - t < nanos do {}
    }
  }

  "Cancellation releases lock only after IO has been canceled" in:
    val lock = AsyncLock("CANCEL-BODY", logWorryDurations = Nil)
    val ioStarted = Deferred.unsafe[IO, Unit]
    @volatile var isCompleted = false

    val run =
      lock.lock:
        for
          _ <- ioStarted.complete(())
          _ <- IO.never.onCancel:
            for _ <- IO.sleep(300.ms) yield
              isCompleted = true
        yield ()

    for
      fiber <- run.start
      _ <- ioStarted.get
      _ <- IO.sleep(100.ms) // Delay until .onCancel has been started
      _ = assert(!isCompleted)
      _ <- fiber.cancel // Blocks until cancellation is completed
    yield
      assert(isCompleted)

  "isLocked" in:
    val lock = AsyncLock("IS-LOCKED", logWorryDurations = Nil)
    for
      _ <- lock.isLocked.map(is => assert(!is))
      _ <- lock.lock(lock.isLocked.map(is => assert(is)))
      _ <- lock.isLocked.map(is => assert(!is))
    yield succeed

  //"apply" in:
  //  val lock = AsyncLock("APPLY", logWorryDurations = Nil)
  //  for
  //    _ <- lock.isLocked.map(is => assert(!is))
  //    _ <- lock(lock.isLocked.map(is => assert(is)))
  //    _ <- lock.isLocked.map(is => assert(!is))
  //  yield succeed

  "Lock acquisition is cancelable" - {
    "suppressLog=false" in:
      runMyTest(AsyncLock("CANCEL-ACQUIRE", suppressLog = false))

    "suppressLog=true" in:
      runMyTest(AsyncLock("CANCEL-ACQUIRE", suppressLog = true))

    def runMyTest(lock: AsyncLock): IO[Assertion] =
      Deferred[IO, Unit]
        .flatMap { locked => IO
          .race(
            lock.lock:
              locked.complete(()) *> lock.lock(IO.never),
            locked.get *>
              IO.defer(IO.sleep((2 << Random.nextInt(14)).µs)) *>
                lock.isLocked.map(is => assert(is)))
          .void
          .*>(lock.isLocked.map(is => assert(!is)))
          .*>(lock.lock(IO.unit))
        }
        .replicateA_(if isIntelliJIdea then 1000 else 100)
        .as(succeed)
  }


object AsyncLockTest:
  private val logger = Logger[this.type]
