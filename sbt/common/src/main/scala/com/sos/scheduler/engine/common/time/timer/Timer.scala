package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.common.time.timer.Timer._
import java.time.Instant
import scala.concurrent._
import scala.util.control.NoStackTrace
import scala.util.{Failure, Try}

/**
  * @author Joacim Zschimmer
  */
final class Timer[A] private[timer](
  val at: Instant,
  val name: String,
  completeWith: Try[A] = ElapsedFailure,
  protected val promise: Promise[A] = Promise[A]())
extends PromiseFuture[A] {

  private[timer] val atEpochMilli = at.toEpochMilli

  private[timer] def complete(): Unit = promise.tryComplete(completeWith)

  private[timer] def cancel(): Boolean = promise.tryComplete(CanceledFailure)

  def onElapsed(body: ⇒ Unit)(implicit ec: ExecutionContext): this.type = {
    onComplete {
      case ElapsedFailure ⇒ body
      case _ ⇒
    }
    this
  }

  override def toString = s"Timer($at $name)"

  def isCanceled = promise.future.value == Some(CanceledFailure)
}

object Timer {
  private[timer] def nowMillis = System.currentTimeMillis

  final class CanceledException private[Timer] extends RuntimeException with NoStackTrace
  val CanceledFailure = Failure(new CanceledException)

  final class ElapsedException private[Timer] extends RuntimeException with NoStackTrace
  val ElapsedFailure = Failure(new ElapsedException)
}
