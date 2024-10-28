package js7.common.concurrent

import java.util.concurrent.Executors.newFixedThreadPool
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.measureTime
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class ThrottledExecutionContextTest extends OurTestSuite, BeforeAndAfterAll:

  private lazy val threadPool = newFixedThreadPool(3)
  private lazy val myExecutionContext = ExecutionContext.fromExecutorService(threadPool)

  override protected def afterAll() =
    threadPool.shutdownNow()
    super.afterAll()

  "With slow operation" in:
    val range = 1 to 1000
    val throttle = 3
    implicit val throttledExecutionContext = new ThrottledExecutionContext(throttle)(myExecutionContext)
    ParallelismCounter.expect(throttle, range.size) { count =>
      val futures =
        for i <- range yield
          Future:
            count:
              if i % 3 == 0 then sleep(Random.nextInt(10).ms)
              i
      assert(futures.await(60.s).sum == range.sum)
    }

  "With quick operation" in:
    for n <- Array(1/*warm-up*/, 1, 2) do
      for throttle <- 1 to 3 do
        info(s"n=$n throttle=$throttle: " +
          measureTime(10000, "FutureThrottle") {
            implicit val throttledExecutionContext = new ThrottledExecutionContext(throttle)(myExecutionContext)
            val futures = for i <- 1 to n yield Future { i }
            assert(futures.await(60.s).sum == (1 to n).sum)
          }.toString)

  if false then
  "Ordinary Future" in:
    given ExecutionContext = ioRuntime.compute

    for n <- 1 to 2 do
      info(s"n=$n: " +
        measureTime(10000, "Future") {
          val futures = for i <- 1 to n yield Future { i }
          assert(futures.await(60.s).sum == (1 to n).sum)
        }.toString)
