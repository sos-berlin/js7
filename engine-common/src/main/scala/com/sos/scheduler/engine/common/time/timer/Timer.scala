package com.sos.scheduler.engine.common.time.timer

import java.time.Instant
import scala.concurrent._

/**
  * @author Joacim Zschimmer
  */
final class Timer[A](val at: Instant, val name: String, protected[timer] val call: () ⇒ A)(implicit ec: ExecutionContext)
extends PromiseFuture[A] {

  protected val promise = Promise[A]()
  private[timer] val atEpochMilli = at.toEpochMilli

  private[timer] def run(): Unit = Future { call.apply() } onComplete promise.complete

  override def toString = s"Timer($at $name)"
}

object Timer {
  def apply[A](at: Instant, name: String, call: () ⇒ A)(implicit ec: ExecutionContext) = new Timer(at, name, call)

  private[timer] def nowMillis() = System.currentTimeMillis
}
