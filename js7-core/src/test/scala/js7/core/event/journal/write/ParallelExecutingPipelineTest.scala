package js7.core.event.journal.write

import java.util.concurrent.Executors
import js7.base.time.ScalaTime._
import js7.common.concurrent.ParallelismCounter
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ParallelExecutingPipelineTest extends AnyFreeSpec
{
  private val sleepDuration = 200.millis

  "ParallelExecutionPipeline" in {
    val threadPool = Executors.newFixedThreadPool(sys.runtime.availableProcessors)
    val executionContext = ExecutionContext.fromExecutor(threadPool)
    val n = 2 * sys.runtime.availableProcessors
    val result = mutable.ArrayBuffer[Int]()
    result.sizeHint(n)
    val pipeline = new ParallelExecutingPipeline[Int](result += _)(executionContext)
    val count = new ParallelismCounter
    def f(i: Int, duration: FiniteDuration) = count {
      sleep(duration)
      i
    }
    val runningSince = now
    for (i <- 1 to n) {
       pipeline.blockingAdd {
         f(i, sleepDuration)
       }
    }
    pipeline.flush()
    threadPool.shutdownNow()
    assert(count.maximum == sys.runtime.availableProcessors)
    val duration = runningSince.elapsed
    assert(duration < n * sleepDuration * 3 / sys.runtime.availableProcessors)
    assert(result == (1 to n))
  }
}
