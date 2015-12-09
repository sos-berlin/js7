package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.common.time.timer.Timer.CancelledException
import java.time.Instant
import scala.concurrent._

/**
  * @author Joacim Zschimmer
  */
final class Timer(val at: Instant, val name: String)
extends PromiseFuture[Unit] {

  protected val promise = Promise[Unit]()
  private[timer] val atEpochMilli = at.toEpochMilli

  private[timer] def fulfill(): Unit = promise.trySuccess(())

  private[timer] def cancel(): Boolean = promise.tryFailure(CancelledException)

  def cancelWhenCompleted[A](future: Future[A])(implicit ec: ExecutionContext): this.type = {
    future onComplete { case _ ⇒ cancel() }
    this
  }

  def then_(body : ⇒ Unit)(implicit ec: ExecutionContext): this.type = {
    onSuccess { case _ ⇒ body }
    this
  }

  override def toString = s"Timer($at $name)"
}

object Timer {
  def apply[A](at: Instant, name: String) = new Timer(at, name)

  private[timer] def nowMillis() = System.currentTimeMillis

  final class CancelledException extends RuntimeException
  val CancelledException = new CancelledException
}
