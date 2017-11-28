package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class ParallelExecutingPipeline[A](output: A ⇒ Unit) {
  // Replace with Akka Streams ?

  private val writeQueue = new ArrayBlockingQueue[Future[A]](sys.runtime.availableProcessors + 1)

  /**
    * Blocks until the queue contains not more than `sys.runtime.availableProcessors` entries.
    */
  def blockingAdd(a: ⇒ A): Unit = {
    while (writeQueue.remainingCapacity == 0 || (!writeQueue.isEmpty && writeQueue.peek.isCompleted)) {
      writeNext()
    }
    writeQueue.put(Future { a } )
  }

  def flush(): Unit = {
    while (!writeQueue.isEmpty) {
      writeNext()
    }
  }

  private def writeNext(): Unit = {
    output(writeQueue.poll.awaitInfinite)
  }
}
