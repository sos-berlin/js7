package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class ParallelExecutingPipeline[A](output: A ⇒ Unit)(implicit executionContext: ExecutionContext) {

  private val writeQueue = new ArrayBlockingQueue[Future[A]](sys.runtime.availableProcessors + 1)

  def add(a: ⇒ A): Unit = {
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
