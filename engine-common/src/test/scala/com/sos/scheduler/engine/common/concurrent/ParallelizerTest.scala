package com.sos.scheduler.engine.common.concurrent

import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch
import java.util.concurrent.Executors
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.{ExecutionContext, Future, blocking}

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ParallelizerTest extends FreeSpec with BeforeAndAfterAll {

  private lazy val threadMaximum = sys.runtime.availableProcessors + 3
  private lazy val parallelThreadPool = Executors.newFixedThreadPool(threadMaximum)  // Enough threads for Futures of this test
  private lazy val parallelExecutionContext = ExecutionContext.fromExecutorService(parallelThreadPool)

  override protected def afterAll() = {
    parallelThreadPool.shutdownNow()
    super.afterAll()
  }

  "Parallelizer" in {
    var sum = 0
    val parallel = Parallelizer.to { o: Int ⇒ sum += o } (parallelExecutionContext)
    val range = 1 to 999
    for (i ← range) parallel { i }
    parallel.finish()
    assert(sum == (1 to 999).sum)
  }

  "Exception is thrown" in {
    val parallel = Parallelizer.to { o: Int ⇒ throw new RuntimeException("ERROR") } (parallelExecutionContext)
    parallel { 1 }
    intercept[RuntimeException] {
      parallel.finish()
    }
  }

  "Maximum parallelization" in {
    val sum = ParallelizationCounter.expect(sys.runtime.availableProcessors) { count ⇒
      parallelSum(count)
    }
    val serialSum = Stopwatch.measureTime("serial") {
      (for (i ← 1 to 50) yield slowOperation(i)).sum
    }
    assert(sum == serialSum)
  }

  "Multiple parallelizations are limited to used ExecutionContext" in {
    ParallelizationCounter.expect(threadMaximum) { count ⇒
      import ExecutionContext.Implicits.global
      (for (_ ← 1 to 10) yield
        Future {
          blocking {
            parallelSum(count)
          }
        }
      ) await 60.s
    }
  }

  private def parallelSum(count: ParallelizationCounter): Int= {
    var sum = 0
    Stopwatch.measureTime("parallel") {
      val parallel = Parallelizer(timeout = 5.s, processResult = (o: Int) ⇒ sum += o) (parallelExecutionContext)
      for (i ← 1 to 50) {
        parallel {
          count {
            slowOperation(i)
          }
        }
      }
      parallel.finish()
    }
    assert(sum == (1 to 50 map 1.+).sum)
    sum
  }

  private def slowOperation(i: Int) = {
    sleep(10.ms)
    i + 1
  }
}
