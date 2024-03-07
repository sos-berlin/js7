package js7.base.thread

import com.typesafe.config.Config
import java.util.concurrent.{ArrayBlockingQueue, ExecutorService, LinkedBlockingQueue, SynchronousQueue, ThreadFactory, ThreadPoolExecutor}
import js7.base.log.Logger
import js7.base.system.Java8Polyfill.*
import js7.base.thread.VirtualThreads.maybeNewVirtualThreadExecutorService
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object ThreadPoolsBase:
  private val logger = Logger[this.type]

  def newBlockingExecutor(name: String, config: Config, virtual: Boolean = false)
  : ExecutorService =
    val keepAlive = config.getDuration("js7.thread-pools.blocking.keep-alive").toFiniteDuration
    val virtualAllowed = virtual && config.getBoolean("js7.thread-pools.virtual-allowed")
    if virtualAllowed then
      maybeNewVirtualThreadExecutorService() getOrElse
        newBlockingNonVirtualExecutor(name, keepAlive)
    else
      newBlockingNonVirtualExecutor(name, keepAlive)

  def newBlockingNonVirtualExecutor(name: String, keepAlive: FiniteDuration = 60.s): ExecutorService =
    newThreadPoolExecutor(name = name, keepAlive = keepAlive,
      corePoolSize = 0, maximumPoolSize = Int.MaxValue, queueSize = Some(0))

  private def newThreadPoolExecutor(
    name: String,
    keepAlive: FiniteDuration,
    corePoolSize: Int = 0,
    maximumPoolSize: Int,
    queueSize: Option[Int])
  : ThreadPoolExecutor =
    val result = new ThreadPoolExecutor(
      corePoolSize,
      maximumPoolSize,
      keepAlive.toMillis,
      MILLISECONDS,
      queueSize match {
        case None => new LinkedBlockingQueue[Runnable]
        case Some(0) => new SynchronousQueue[Runnable](false)  // Like Monix Scheduler.io
        case Some(n) => new ArrayBlockingQueue[Runnable](n)
      },
      myThreadFactory(name))
    logger.debug(s"newThreadPoolExecutor => $result")
    result

  private def myThreadFactory(name: String): ThreadFactory =
    runnable => {
      val thread = new Thread(runnable)
      thread.setName(s"$name-${thread.threadId}")
      thread.setDaemon(true)  // Do it like Monix and Pekko
      thread
    }

  java8Polyfill()
