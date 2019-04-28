package com.sos.jobscheduler.core.event.journal.write

import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.concurrent.ParallelismCounter
import com.sos.jobscheduler.common.time.ScalaTime._
import java.util.concurrent.Executors
import org.scalatest.FreeSpec
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ParallelExecutingPipelineTest extends FreeSpec
{
  private val sleepDuration = 200.millis

  "ParallelExecutionPipeline" in {
    val threadPool = Executors.newFixedThreadPool(sys.runtime.availableProcessors)
    val executionContext = ExecutionContext.fromExecutor(threadPool)
    val n = 2 * sys.runtime.availableProcessors
    val result = mutable.ArrayBuffer[Int]()
    result.sizeHint(n)
    val pipeline = new ParallelExecutingPipeline[Int](result.+=)(executionContext)
    val count = new ParallelismCounter
    def f(i: Int, duration: FiniteDuration) = count {
      sleep(duration)
      i
    }
    val t = now
    for (i <- 1 to n) {
       pipeline.blockingAdd {
         f(i, sleepDuration)
       }
    }
    pipeline.flush()
    threadPool.shutdownNow()
    assert(count.maximum == sys.runtime.availableProcessors)
    val duration = now - t
    assert(duration < n * sleepDuration * 3 / sys.runtime.availableProcessors)
    assert(result == (1 to n))
  }
}
