package com.sos.scheduler.engine.common.time.alarm

import com.sos.scheduler.engine.common.scalautil.Collections.emptyToNone
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final case class Alarm(at: Instant, name: () ⇒ String, call: () ⇒ Unit)(implicit ec: ExecutionContext) {

  private[alarm] val atEpochMilli = at.toEpochMilli

  private[alarm] def run(): Unit = Future { call.apply() }

  override def toString = s"Alarm(" + (Some(at) ++ emptyToNone(name()) mkString ": ") + ")"
}

object Alarm {
  private[alarm] def nowMillis() = System.currentTimeMillis
}
