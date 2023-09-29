package js7.common.concurrent

import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class ThrottledExecutionContext(throttle: Int)(implicit delegate: ExecutionContext)
extends ExecutionContext:

  require(throttle >= 1)

  private val semaphore = new Semaphore(throttle)
  private val queue = new ConcurrentLinkedQueue[Runnable]
  // Two more behaviours are imaginable:
  // - blocking: with ArrayBlockingQueue, execute() blocks when a limit is reached
  // - rejecting: with a limited ConcurrentLinkedQueue, execute() fails when a limit is reached

  def reportFailure(throwable: Throwable) = delegate.reportFailure(throwable)

  def execute(runnable: Runnable) =
    if !enqueueIfThrottled(runnable) then
      delegate.execute(new Runnable {
        def run() = executeThisAndQueued(runnable)
      })

  private def executeThisAndQueued(runnable: Runnable): Unit =
    delegate.execute(new Runnable {   // Avoid memory-leak with runnable.
      def run() = {
        runnable.run()
        tryDequeue() match {
          case null =>
          case next => executeThisAndQueued(next)
        }
      }
    })

  private def enqueueIfThrottled[A](runnable: Runnable): Boolean =
    synchronized:
      !semaphore.tryAcquire() && {
        queue.add(runnable)
        true
      }

  private def tryDequeue(): Runnable =
    synchronized:
      val next = queue.poll()
      if next == null then
        semaphore.release()
      next
