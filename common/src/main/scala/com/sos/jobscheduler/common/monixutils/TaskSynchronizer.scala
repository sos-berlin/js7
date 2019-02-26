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
private[monixutils]  // NOT USED
final class TaskSynchronizer[A](limit: Int = Int.MaxValue)(implicit scheduler: Scheduler)
{
  requireNonNull(scheduler)  // May occur on lazy val scheduler

  private val queue = new java.util.concurrent.LinkedBlockingQueue[(Promise[Option[A]], Task[A])](limit)
  private val isExecuting = AtomicBoolean(false)

  def run(task: Task[A]): Future[Option[A]] = {
    requireNonNull(task)
    promiseFuture[Option[A]] { promise =>
      val ok = queue.offer(promise -> task)
      if (!ok) {
        promise.success(None)
      } else {
        runNext()
      }
    }
  }

  private def runNext(): Unit =
    if (!isExecuting.getAndSet(true)) {
      queue.poll() match {
        case null =>
          isExecuting := false

        case (promise, task) =>
          task.runAsync { outcome =>
            isExecuting := false
            runNext()
            promise.complete(outcome.toTry map Some.apply)
          }
      }
    }
}
