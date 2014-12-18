package com.sos.scheduler.engine.common.async

import com.sos.scheduler.engine.common.async.CallQueue._
import com.sos.scheduler.engine.common.scalautil.Logger
import java.util.NoSuchElementException
import org.joda.time.Instant
import scala.concurrent.ExecutionContext

trait CallQueue extends AutoCloseable {

  object executionContext extends ExecutionContext {
    def execute(o: Runnable): Unit = {
      add(o)
    }

    def reportFailure(t: Throwable): Unit = {
      // Should never be called, because NonFatal exceptions are already handled by class Call.
      logger.error(t.toString, t)
    }
  }

  object implicits {
    implicit val executionContext: ExecutionContext = CallQueue.this.executionContext
  }

  def at[A](t: Instant)(f: ⇒ A): TimedCall[A] = {
    val call = TimedCall(t)(f)
    add(call)
    call
  }

  def apply(f: ⇒ Unit): Unit = {
    add(ShortTermCall { () ⇒ f })
  }

  def add[A](o: TimedCall[A]): Unit

  def tryCancel[A](o: TimedCall[A]): Boolean

  final def add(o: Runnable): Unit = {
    add(ShortTermCall(o))
  }

  final def remove(o: TimedCall[_]): Unit = {
    val removed = tryCancel(o)
    if (!removed) throw new NoSuchElementException(s"Unknown TimedCall '$o'")
  }

  def nextTime: Long
}

object CallQueue {
  private val logger = Logger(getClass)

  final class ClosedException(override val getMessage: String) extends IllegalStateException
}
