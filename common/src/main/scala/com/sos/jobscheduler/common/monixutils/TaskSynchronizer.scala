package com.sos.jobscheduler.common.monixutils

import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import java.util.Objects.requireNonNull
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicBoolean
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class TaskSynchronizer[A]()(implicit scheduler: Scheduler)
{
  requireNonNull(scheduler)  // May occur on lazy val scheduler

  private val queue = new java.util.concurrent.LinkedBlockingQueue[(Promise[A], Task[A])]
  private val isExecuting = AtomicBoolean(false)

  def run(task: Task[A]): Future[A] = {
    requireNonNull(task)
    promiseFuture[A] { promise ⇒
      queue.add(promise → task)
      runNext()
    }
  }

  private def runNext(): Unit =
    if (!isExecuting.getAndSet(true)) {
      queue.poll() match {
        case null ⇒
          isExecuting := false

        case (promise, task) ⇒
          task.runOnComplete { tried ⇒
            isExecuting := false
            runNext()
            promise.complete(tried)
          }
      }
    }
}
