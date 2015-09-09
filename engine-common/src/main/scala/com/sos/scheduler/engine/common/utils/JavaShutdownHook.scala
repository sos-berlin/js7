package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.utils.JavaShutdownHook._
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class JavaShutdownHook private(onShutdown: () ⇒ Unit, name: String) extends AutoCloseable {

  private val hook = new Thread {
    setName(name)
    override def run() = onShutdown()
  }

  Runtime.getRuntime.addShutdownHook(hook)

  def close(): Unit =
    try Runtime.getRuntime.removeShutdownHook(hook)
    catch {
      case t: IllegalStateException ⇒ logger.trace(s"removeShutdownHook: $t", t) // "Shutdown in progress"
      case NonFatal(t) ⇒ logger.warn(s"removeShutdownHook: $t", t)
    }
}

object JavaShutdownHook {
  private val logger = Logger(getClass)

  def add(onShutdown: () ⇒ Unit, name: String) = new JavaShutdownHook(onShutdown, name)

  def add(name: String)(atShutdown: ⇒ Unit) = new JavaShutdownHook(() ⇒ atShutdown, name)
}
