package com.sos.scheduler.engine.common.utils

import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object Exceptions {
  def ignoreException(log: (⇒ String, Throwable) ⇒ Unit)(body: ⇒ Unit): Unit =
    try body
    catch {
      case NonFatal(t) ⇒ log(s"Ignoring exception $t", t)
    }
}
