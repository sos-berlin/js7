package com.sos.jobscheduler.common.concurrent

import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import java.util.concurrent.Executors.newFixedThreadPool
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ThrottledExecutionContextTest extends AnyFreeSpec with BeforeAndAfterAll {

  private lazy val threadPool = newFixedThreadPool(3)
  private lazy val executionContext = ExecutionContext.fromExecutorService(threadPool)

  override protected def afterAll() = {
    threadPool.shutdownNow()
    super.afterAll()
  }

  "With slow operation" in {
    val range = 1 to 1000
    val throttle = 3
    implicit val throttledExecutionContext = new ThrottledExecutionContext(throttle)(executionContext)
    ParallelismCounter.expect(throttle, range.size) { count =>
      val futures =
        for (i <- range) yield {
          Future {
            count {
              if (i % 3 == 0) sleep(Random.nextInt(10).ms)
              i
            }
          }
        }
      assert((futures await 60.s).sum == range.sum)
    }
  }

  "With quick operation" in {
    for (n <- Array(1/*warm-up*/, 1, 2)) {
      for (throttle <- 1 to 3) {
        info(s"n=$n throttle=$throttle: " +
          measureTime(10000, "FutureThrottle") {
            implicit val throttledExecutionContext = new ThrottledExecutionContext(throttle)(executionContext)
            val futures = for (i <- 1 to n) yield Future { i }
            assert((futures await 60.s).sum == (1 to n).sum)
          }.toString)
      }
    }
  }

  if (false)
  "Ordinary Future" in {
    import ExecutionContext.Implicits.global
    for (n <- 1 to 2) {
      info(s"n=$n: " +
        measureTime(10000, "Future") {
          val futures = for (i <- 1 to n) yield Future { i }
          assert((futures await 60.s).sum == (1 to n).sum)
        }.toString)
    }
  }
}
