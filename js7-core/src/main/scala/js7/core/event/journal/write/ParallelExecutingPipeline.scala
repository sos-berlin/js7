package js7.core.event.journal.write

import java.util.concurrent.ArrayBlockingQueue
import js7.common.scalautil.Futures.implicits.SuccessFuture
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class ParallelExecutingPipeline[A](output: A => Unit)(implicit ec: ExecutionContext) {

  private val queue = new ArrayBlockingQueue[Future[A]](sys.runtime.availableProcessors)

  /**
    * Blocks until the queue contains not more than `sys.runtime.availableProcessors` entries.
    */
  def blockingAdd(a: => A): Unit = {
    if (queue.remainingCapacity == 0) {
      writeNext()
    }
    queue.put(Future { a } )
    while (!queue.isEmpty && queue.peek.isCompleted) {
      writeNext()
    }
  }

  def flush(): Unit =
    while (!queue.isEmpty) writeNext()

  private def writeNext(): Unit =
    output(queue.poll.awaitInfinite)
}
