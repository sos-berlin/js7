package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaShutdownHook._
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

  def close(): Unit = remove()

  def remove(): Unit =
    try Runtime.getRuntime.removeShutdownHook(hook)
    catch {
      case t: IllegalStateException ⇒ logger.debug(s"removeShutdownHook: $t")  // "Shutdown in progress"
      case NonFatal(t) ⇒ logger.warn(s"removeShutdownHook: $t", t)
    }
}

object JavaShutdownHook {
  private val logger = Logger(getClass)

  def add(onShutdown: () ⇒ Unit, name: String) = new JavaShutdownHook(onShutdown, name)

  def add(name: String)(atShutdown: ⇒ Unit) = new JavaShutdownHook(() ⇒ atShutdown, name)
}
