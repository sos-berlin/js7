package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.Instant.now
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class ParallelExecutingPipelineTest extends FreeSpec {

  "ParallelExecutionPipeline" in {
    val result = mutable.Buffer[Int]()
    val pipeline = new ParallelExecutingPipeline[Int](result.+=)
    val currentParallelism = new AtomicInteger
    val maxParallelism = new AtomicInteger
    def f(i: Int) = {
      val p = currentParallelism.incrementAndGet()
      maxParallelism.compareAndSet(p - 1, p)
      Thread.sleep(1)
      currentParallelism.decrementAndGet()
      i
    }
    pipeline.blockingAdd {  // Warm-up
      f(0)
    }
    val n = 1000
    val t = now
    for (i ← 1 to n) {
       pipeline.blockingAdd {
         f(i)
       }
    }
    pipeline.flush()
    assert(maxParallelism.get == sys.runtime.availableProcessors)
    val duration = now - t
    assert(duration < 2 * n * 1.ms / sys.runtime.availableProcessors)
    assert(result == (0 to n))
  }
}
