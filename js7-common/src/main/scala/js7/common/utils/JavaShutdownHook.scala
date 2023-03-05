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
    logger.debugCall(s"⚡️ Shutdown hook '$name'", "") {
      onShutdown()
      // End may not be logged if log4j has been shutdown
    }
  }

  sys.runtime.addShutdownHook(hook)
  logger.trace(s"JavaShutdownHook '$name' added")

  def close(): Unit = remove()

  def remove(): Unit =
    try sys.runtime.removeShutdownHook(hook)
    catch {
      case t: IllegalStateException => logger.trace(s"JavaShutdownHook.remove: ${t.toStringWithCauses}")  // "Shutdown in progress"
      case NonFatal(t) => logger.warn(s"JavaShutdownHook.remove: $t", t)
    }
}

object JavaShutdownHook {
  private val logger = Logger(getClass)

  def add(onShutdown: () => Unit, name: String) = new JavaShutdownHook(onShutdown, name)

  def add(name: String)(atShutdown: => Unit) = new JavaShutdownHook(() => atShutdown, name)
}
