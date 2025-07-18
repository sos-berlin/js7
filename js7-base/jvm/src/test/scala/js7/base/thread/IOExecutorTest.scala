package js7.base.thread

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Outcome}
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import java.util.concurrent.ExecutorService
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.ThreadPoolsBase.newBlockingNonVirtualExecutor
import js7.base.thread.VirtualThreads.maybeNewVirtualThreadExecutorService
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import scala.concurrent.duration.Deadline.now
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class IOExecutorTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  private val logger = Logger[this.type]

  "Success" in:
    assert(IOExecutor.blocking(7).await(1.s) == 7)

  "Failure" in:
    assert(IOExecutor.blocking(sys.error("FAILED")).attempt.await(1.s).left.toOption.get.toString ==
      "java.lang.RuntimeException: FAILED")

  if !VirtualThreads.isEnabled then
    "Thread name" in:
      assert:
        IOExecutor.blocking(Thread.currentThread.getName: String)
          .await(99.s)
          .startsWith("JS7 global I/O-")

  "interruptible" in:
    val expectedThreadPrefix = (!VirtualThreads.isEnabled) ? (IOExecutor.globalName + "-")
    val test =
      val exception = Exception("TEST")
      for
        succeedingFiber <-
          IOExecutor.interruptible:
            expectedThreadPrefix match
              case None => assert(Thread.currentThread.getName.isEmpty)
              case Some(prefix) => assert(Thread.currentThread.getName.startsWith(prefix))
            Thread.sleep(Random.nextInt(2))
            3
          .start
        failingFiber <- IOExecutor.interruptible(throw exception).start
        canceledFiber <- IOExecutor.interruptible(Thread.sleep(999999)).start

        successOutcome <- succeedingFiber.join
        failedOutcome <- failingFiber.join

        _ <- IO.sleep(Random.nextInt(5).ms)
        _ <- canceledFiber.cancel
        canceledOutcome <- canceledFiber.join
      yield
        assert(successOutcome == Outcome.Succeeded(IO.pure(3)) &&
          failedOutcome == Outcome.Errored(exception) &&
          canceledOutcome == Outcome.Canceled())

    test.parReplicateA_(1000).as(succeed)

  if sys.props.contains("test.speed") then
    if VirtualThreads.isEnabled then
      "Performance with VirtualThread" in:
        for executor <- maybeNewVirtualThreadExecutorService() do
          testPerformance(executor, 1000)
          testPerformance(executor, 10000)
          testPerformance(executor, 100000)
          testPerformance(executor, 200000)
          testPerformance(executor, 1000000) // May require big heap
          executor.shutdown()
        succeed

    "Performance with thread pool" in:
      val executor = newBlockingNonVirtualExecutor("IOExecutorTest")
      testPerformance(executor, 1000)
      testPerformance(executor, 10000)
      testPerformance(executor, 100000)
      executor.shutdown()
      succeed

  private def testPerformance(executor: ExecutorService, n: Int): Unit =
    val iox = new IOExecutor(executor, "IOExecutorTest")
    val since = now
    val io = (1 to n).toVector
      .traverse(_ => IOExecutor.blocking(sleep(100.ms)).start)
      .map(_.map(_.joinStd))
      .map(_.combineAll)
      .flatten
    for i <- 1 to 3 do
      io.await(99.s)
      logger.info(itemsPerSecondString(since.elapsed, n))
