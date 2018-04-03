package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.concurrent.ParallelismCounter
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.ParallelExecutingPipelineExclusiveTest._
import java.time.Instant.now
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class ParallelExecutingPipelineExclusiveTest extends FreeSpec {

  private val sleepDuration = 1.ms

  "ParallelExecutionPipeline" in {
    val n = 500 * sys.runtime.availableProcessors
    val result = mutable.ArrayBuffer[Int]()
    result.sizeHint(n)
    val pipeline = new ParallelExecutingPipeline[Int](result.+=)
    val count = new ParallelismCounter
    def f(i: Int) = count {
      nanoLoop(sleepDuration.toNanos)
      i
    }
    pipeline.blockingAdd {  // Warm-up
      f(0)
    }
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

object ParallelExecutingPipelineExclusiveTest {
  private def nanoLoop(nanos: Long): Unit = {
    val t = System.nanoTime + nanos
    while (System.nanoTime < t) {}
  }
}
