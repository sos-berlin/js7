package com.sos.scheduler.engine.common.concurrent

import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch
import java.util.concurrent.Executors.newFixedThreadPool
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class ThrottledExecutionContextTest extends FreeSpec with BeforeAndAfterAll {

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
    ParallelismCounter.expect(throttle, range.size) { count ⇒
      val futures =
        for (i ← range) yield {
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
    for (n ← Array(1/*warm-up*/, 1, 2)) {
      for (throttle ← 1 to 3) {
        Stopwatch.measureTime(10000, "FutureThrottle", linePrefix = s"n=$n throttle=$throttle: ") {
          implicit val throttledExecutionContext = new ThrottledExecutionContext(throttle)(executionContext)
          val futures = for (i ← 1 to n) yield Future { i }
          assert((futures await 60.s).sum == (1 to n).sum)
        }
      }
    }
  }

  if (false)
  "Ordinary Future" in {
    import ExecutionContext.Implicits.global
    for (n ← 1 to 2) {
      Stopwatch.measureTime(10000, "Future", linePrefix = s"n=$n: ") {
        val futures = for (i ← 1 to n) yield Future { i }
        assert((futures await 60.s).sum == (1 to n).sum)
      }
    }
  }
}
