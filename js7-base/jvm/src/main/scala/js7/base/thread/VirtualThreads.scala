package js7.base.thread

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SystemPropertiesExtensions.asSwitch

object VirtualThreads:

  private val logger = Logger[this.type]
  private var hasVirtualThreads =
    Runtime.version.feature >= 24 && sys.props.asSwitch("js7.virtualThreads")
  private val DefaultVirtualThreadMaxPoolSize = 999_999  // Should be virtually unlimited

  private lazy val maybeNewVirtualThreadPerTaskExecutor: Option[() => ExecutorService] =
    for
      factory <- newVirtualThreadFactory
      newThreadPerTaskExecutor <- newThreadPerTaskExecutor
    yield
      () => newThreadPerTaskExecutor(factory)

  logger.debug(s"hasVirtualThreads=$hasVirtualThreads")

  def isEnabled: Boolean =
    maybeNewVirtualThreadPerTaskExecutor.isDefined

  private[thread] def maybeNewVirtualThreadExecutorService(): Option[ExecutorService] =
    maybeNewVirtualThreadPerTaskExecutor.map(_())

  def newMaybeVirtualThread(name: String = "")(body: => Unit): Thread =
    _newMaybeVirtualThread(name)(() => body)

  private lazy val _newMaybeVirtualThread: String => Runnable => Thread =
    newVirtualThreadFactory match
      case Some(factory) =>
        _ => runnable => factory.newThread(runnable)

      case None =>
        name => runnable =>
          val thread = new Thread(runnable)
          if name.nonEmpty then thread.setName(name)
          thread

  private lazy val newThreadPerTaskExecutor: Option[ThreadFactory => ExecutorService] =
    hasVirtualThreads thenMaybe:
      setBlockingVirtualThreadPoolSize(DefaultVirtualThreadMaxPoolSize)
      try
        val method = classOf[Executors]
          .getMethod("newThreadPerTaskExecutor", classOf[ThreadFactory])
        Some:
          threadFactory =>
            method.invoke(null, threadFactory).asInstanceOf[ExecutorService]
      catch throwableToNone

  private def setBlockingVirtualThreadPoolSize(n: Int): Unit =
    val name = "jdk.virtualThreadScheduler.maxPoolSize"
    if !sys.props.contains(name) then
      logger.debug(s"Setting missing system property $name=$n")
      sys.props.put(name, n.toString)

  private[thread] lazy val newVirtualThreadFactory: Option[ThreadFactory] =
    hasVirtualThreads thenMaybe:
      try
        val builder = classOf[Thread].getMethod("ofVirtual").invoke(null)
        val factory = Class
          .forName("java.lang.Thread$Builder")
          .getMethod("factory")
          .invoke(builder)
          .asInstanceOf[ThreadFactory]

        testThreadFactory(factory)
        logger.debug(s"newVirtualThreadFactory => $factory")
        Some(factory)
      catch throwableToNone

  private def testThreadFactory(factory: ThreadFactory): Unit =
    val newThread: Runnable => Thread = runnable =>
      factory.newThread(runnable)

    val testThread = newThread: () =>
      logger.debug(s"""Using Java VirtualThreads for some operations "${Thread.currentThread}"""")
    testThread.start()
    testThread.join()

  private def throwableToNone: PartialFunction[Throwable, None.type] =
    throwable =>
      hasVirtualThreads = false
      logger.debug(s"No VirtualThread: ${throwable.toStringWithCauses}")
      None
