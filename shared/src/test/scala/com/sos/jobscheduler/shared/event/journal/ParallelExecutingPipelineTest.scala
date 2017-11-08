package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.concurrent.ParallelismCounter
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.Instant.now
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class ParallelExecutingPipelineTest extends FreeSpec {

  private val sleepDuration = 10.ms

  "ParallelExecutionPipeline" in {
    val result = mutable.Buffer[Int]()
    val pipeline = new ParallelExecutingPipeline[Int](result.+=)
    val count = new ParallelismCounter
    def f(i: Int) = count {
      sleep(sleepDuration)
      i
    }
    pipeline.blockingAdd {  // Warm-up
      f(0)
    }
    val n = 20 * sys.runtime.availableProcessors
    val t = now
    for (i ← 1 to n) {
       pipeline.blockingAdd {
         f(i)
       }
    }
    pipeline.flush()
    assert(count.maximum == sys.runtime.availableProcessors)
    val duration = now - t
    assert(duration < n * sleepDuration * 3 / sys.runtime.availableProcessors)
    assert(result == (0 to n))
  }
}
