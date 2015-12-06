package com.sos.scheduler.engine.common.time.alarm

import com.sos.scheduler.engine.common.scalautil.Collections.emptyToNone
import java.time.Instant
import scala.concurrent._

/**
  * @author Joacim Zschimmer
  */
final case class Alarm[A](at: Instant, name: String, call: () ⇒ A)(implicit ec: ExecutionContext)
extends PromiseFuture[A] {

  protected val promise = Promise[A]()
  private[alarm] val atEpochMilli = at.toEpochMilli

  private[alarm] def run(): Unit = Future { call.apply() } onComplete promise.complete

  override def toString = s"Alarm(" + (Some(at) ++ emptyToNone(name) mkString ": ") + ")"
}

object Alarm {
  private[alarm] def nowMillis() = System.currentTimeMillis
}
