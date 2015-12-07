package com.sos.scheduler.engine.common.time.alarm

import java.time.Instant
import scala.concurrent._

/**
  * @author Joacim Zschimmer
  */
final class Alarm[A](val at: Instant, val name: String, protected[alarm] val call: () ⇒ A)(implicit ec: ExecutionContext)
extends PromiseFuture[A] {

  protected val promise = Promise[A]()
  private[alarm] val atEpochMilli = at.toEpochMilli

  private[alarm] def run(): Unit = Future { call.apply() } onComplete promise.complete

  override def toString = s"Alarm($at $name)"
}

object Alarm {
  def apply[A](at: Instant, name: String, call: () ⇒ A)(implicit ec: ExecutionContext) = new Alarm(at, name, call)

  private[alarm] def nowMillis() = System.currentTimeMillis
}
