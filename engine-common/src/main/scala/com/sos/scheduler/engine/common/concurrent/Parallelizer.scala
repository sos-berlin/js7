package com.sos.scheduler.engine.common.concurrent

import com.sos.scheduler.engine.base.utils.StackTraces._
import com.sos.scheduler.engine.common.concurrent.Parallelizer._
import com.sos.scheduler.engine.common.time.ScalaTime.RichDuration
import java.time.Duration
import java.util.concurrent.TimeUnit.MILLISECONDS
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Semaphore, TimeoutException}
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * Parallelizes execution and returns results on caller's thread for further processing.
  */
abstract class Parallelizer[A](parallelization: Int, timeout: Duration)(implicit ec: ExecutionContext) {

  require(parallelization >= 1)

  private val semaphore = new Semaphore(parallelization)
  private val completions = new java.util.concurrent.ConcurrentLinkedQueue[Try[A]]
  private val stopped = new AtomicBoolean

  /**
    * Called on caller's thread, unordered.
    */
  protected def processResult(result: A): Unit

  final def apply(parallelExecution: ⇒ A): Unit = {
    if (stopped.get) throw new IllegalStateException("Parallelizer has been finished")
    processCompletions()
    acquireSemaphore()
    Future { parallelExecution } onComplete { o ⇒
      completions.add(o)
      semaphore.release()
    }
  }

  final def finish() = {
    if (stopped getAndSet true) throw new IllegalStateException("Parallelizer.finished")
    for (_ ← 1 to parallelization) {
      acquireSemaphore()
      processCompletions()
    }
  }

  private def acquireSemaphore(): Unit = {
    if (timeout >= NeverTimeout) {
      semaphore.acquire()
    } else {
      val acquired = semaphore.tryAcquire(timeout.toMillis, MILLISECONDS)
      if (!acquired) throw new TimeoutException(s"Parallelizer timed out after ${timeout.pretty}")
    }
  }

  @tailrec
  final def processCompletions(): Unit = {
    val completion = completions.poll()
    if (completion != null) {
      completion match {
        case Success(o) ⇒
          processResult(o)
          processCompletions()
        case Failure(t) ⇒ throw t.appendCurrentStackTrace
      }
    }
  }
}

object Parallelizer {
  val NeverTimeout = Duration.ofMillis(Long.MaxValue)

  /**
    * @param processResult is called in caller's thread for each result, unordered.
    */
  def to[A](processResult: A ⇒ Unit)(implicit ec: ExecutionContext): Parallelizer[A] =
    apply(processResult = processResult)

  /**
    * @param parallelization Maximum of parallel executions on executionContext
    * @param timeout for a parallel operation. Use it in case of deadlock with full ExecutionContext.
    * @param processResult is called in caller's thread for each result, unordered.
    */
  def apply[A](
    parallelization: Int = sys.runtime.availableProcessors,
    timeout: Duration = NeverTimeout,
    processResult: A ⇒ Unit)
    (implicit executioContext: ExecutionContext)
  : Parallelizer[A] = {
    val processResult_ = processResult
    new Parallelizer[A](parallelization, timeout) {
      def processResult(result: A) = processResult_(result)
    }
  }
}
