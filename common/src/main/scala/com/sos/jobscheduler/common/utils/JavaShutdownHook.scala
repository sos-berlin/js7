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
    override def run() = {
      logger.trace(s"Shutdown hook '$name' starts")
      onShutdown()
      logger.trace(s"Shutdown hook '$name' ends")  // May not be logged if log4j has been shutdown
    }
  }

  sys.runtime.addShutdownHook(hook)
  logger.trace(s"JavaShutdownHook '$name' added")

  def close(): Unit = remove()

  def remove(): Unit =
    try sys.runtime.removeShutdownHook(hook)
    catch {
      case t: IllegalStateException ⇒ logger.trace(s"JavaShutdownHook.remove: $t")  // "Shutdown in progress"
      case NonFatal(t) ⇒ logger.warn(s"JavaShutdownHook.remove: $t", t)
    }
}

object JavaShutdownHook {
  private val logger = Logger(getClass)

  def add(onShutdown: () ⇒ Unit, name: String) = new JavaShutdownHook(onShutdown, name)

  def add(name: String)(atShutdown: ⇒ Unit) = new JavaShutdownHook(() ⇒ atShutdown, name)
}
