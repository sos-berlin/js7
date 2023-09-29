package js7.common.utils

import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.VirtualThreads.newMaybeVirtualThread
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.utils.JavaShutdownHook.*
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class JavaShutdownHook private(onShutdown: () => Unit, name: String) extends AutoCloseable {

  private val hook = newMaybeVirtualThread(s"JavaShutdownHook-$name") {
    shutdown = true
    logger.debugCall(s"⚡️ Shutdown hook '$name'", "") {
      onShutdown()
      // End may not be logged if log4j has been shutdown
    }
  }

  logger.trace(s"$toString addShutdownHook")
  sys.runtime.addShutdownHook(hook)

  def close(): Unit =
    remove()

  def remove(): Unit =
    if !shutdown then {
      try {
        sys.runtime.removeShutdownHook(hook)
        logger.trace(s"$toString ShutdownHook removed")
      } catch {
        case t: IllegalStateException =>
          // "Shutdown in progress" ?
          logger.trace(s"JavaShutdownHook.remove: ${t.toStringWithCauses}")
        case NonFatal(t) =>
          logger.warn(s"JavaShutdownHook.remove: $t", t)
      }
    }

  override def toString = s"JavaShutdownHook($name)"
}

object JavaShutdownHook {
  private val logger = Logger[this.type]
  private var shutdown = false

  def add(onShutdown: () => Unit, name: String) = new JavaShutdownHook(onShutdown, name)

  def add(name: String)(atShutdown: => Unit) = new JavaShutdownHook(() => atShutdown, name)
}
