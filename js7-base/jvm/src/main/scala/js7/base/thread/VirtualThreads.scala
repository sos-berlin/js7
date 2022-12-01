package js7.base.thread

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}
import js7.base.log.Logger
import js7.base.system.Java8Polyfill.javaVersion
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import org.jetbrains.annotations.TestOnly

object VirtualThreads
{
  private val logger = Logger(getClass)
  private var enabled = javaVersion >= 19 && (isTest || sys.props.contains("js7.VirtualThread"))

  private lazy val maybeNewVirtualThreadPerTaskExecutor: Option[() => ExecutorService] =
    for {
      factory <- newVirtualThreadFactory
      newThreadPerTaskExecutor <- newThreadPerTaskExecutor
    } yield
      () => newThreadPerTaskExecutor(factory)

  @TestOnly
  def isEnabled =
    maybeNewVirtualThreadPerTaskExecutor.isDefined

  private[thread] def maybeNewVirtualThreadExecutorService(): Option[ExecutorService] =
    maybeNewVirtualThreadPerTaskExecutor.map(_())

  lazy val newMaybeVirtualThread: Runnable => Thread =
    newVirtualThreadFactory match {
      case Some(factory) => factory.newThread(_)
      case None => new Thread(_)
    }

  private lazy val newThreadPerTaskExecutor: Option[ThreadFactory => ExecutorService] =
    if (!enabled)
      None
    else
      try {
        val method = classOf[Executors]
          .getMethod("newThreadPerTaskExecutor", classOf[ThreadFactory])
        Some(
          threadFactory =>
            method.invoke(null, threadFactory).asInstanceOf[ExecutorService])
      } catch throwableToNone

  private lazy val newVirtualThreadFactory: Option[ThreadFactory] =
    if (!enabled)
      None
    else
      try {
        val builder = classOf[Thread].getMethod("ofVirtual").invoke(null)

        //IllegalAccessException:
        //try builder.getClass
        //  .getMethod("name", classOf[String])
        //  .invoke(builder, "virtual")
        //  .asInstanceOf[ThreadFactory]
        //catch { case t: Throwable => logger.debug(t.toStringWithCauses)}

        val factory = Class
          .forName("java.lang.Thread$Builder")
          .getMethod("factory")
          .invoke(builder)
          .asInstanceOf[ThreadFactory]

        testThreadFactory(factory)
        Some(factory)
      } catch(throwableToNone)

  private def testThreadFactory(factory: ThreadFactory): Unit = {
    val newThread: Runnable => Thread = runnable =>
      factory.newThread(runnable)

    val testThread = newThread { () =>
      logger.debug(
        s"""Using Java 19 VirtualThreads for some operations "${Thread.currentThread}"""")
    }
    testThread.start()
    testThread.join()
  }

  private def throwableToNone: PartialFunction[Throwable, None.type] = {
    throwable =>
      enabled = false
      logger.debug(s"No VirtualThread: ${throwable.toStringWithCauses}")
      None
  }
}
