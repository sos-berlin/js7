package js7.base.thread

import java.util.concurrent.{ExecutorService, Executors}
import js7.base.log.Logger
import js7.base.system.Java8Polyfill.javaVersion
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import monix.execution.atomic.Atomic
import org.jetbrains.annotations.TestOnly

object VirtualThreads
{
  private val logger = Logger(getClass)
  private var enabled = javaVersion >= 19 && (isTest || sys.props.contains("js7.VirtualThread"))
  private val successLogged = Atomic(false)

  private lazy val maybeNewVirtualThreadPerTaskExecutor: Option[() => ExecutorService] =
    if (!enabled) {
      None
    } else
      try {
        val method = classOf[Executors].getMethod("newVirtualThreadPerTaskExecutor")
        def invoke() = method.invoke(null).asInstanceOf[ExecutorService]
        proveUsability(invoke())
        Some(invoke _)
      } catch throwableToNone

  // May throw
  private def proveUsability(executor: ExecutorService): Unit = {
    executor.execute { () =>
      if (!successLogged.getAndSet(true)) {
        logger.debug(
          s"""Using Java 19 VirtualThreads for some operations "${Thread.currentThread}"""")
      }
    }
    executor.shutdown()
  }

  @TestOnly
  def isEnabled =
    maybeNewVirtualThreadPerTaskExecutor.isDefined

  private[thread] def maybeNewVirtualThreadExecutorService(): Option[ExecutorService] =
    maybeNewVirtualThreadPerTaskExecutor.map(_())

  private def throwableToNone: PartialFunction[Throwable, None.type] = {
    throwable =>
      enabled = false
      logger.debug(s"${throwable.toStringWithCauses}")
      None
  }

}
