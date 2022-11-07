package js7.base.thread

import com.typesafe.config.Config
import java.util.concurrent.{ArrayBlockingQueue, ExecutorService, LinkedBlockingQueue, SynchronousQueue, ThreadFactory, ThreadPoolExecutor}
import js7.base.system.Java8Polyfill.*
import js7.base.thread.VirtualThreads.maybeNewVirtualThreadExecutorService
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object ThreadPoolsBase
{
  def newBlockingExecutor(config: Config, name: String): ExecutorService =
    newBlockingExecutor(
      name = name,
      keepAlive = config.getDuration("js7.thread-pools.io.keep-alive").toFiniteDuration)

  def newBlockingExecutor(name: String, keepAlive: FiniteDuration = 60.s): ExecutorService =
    maybeNewVirtualThreadExecutorService() getOrElse
      newBlockingThreadPool(name, keepAlive)

  def newBlockingThreadPool(name: String, keepAlive: FiniteDuration = 60.s): ExecutorService =
    newThreadPoolExecutor(name = name, keepAlive = keepAlive,
      corePoolSize = 0, maximumPoolSize = Int.MaxValue, queueSize = Some(0))

  //def newThreadPoolExecutor(config: Config, name: String): ThreadPoolExecutor =
  //  newThreadPoolExecutor(
  //    name = name,
  //    keepAlive = config.getDuration("js7.thread-pools.io.keep-alive").toFiniteDuration,
  //    corePoolSize = config.getInt("js7.thread-pools.io.core-pool-size"),
  //    maximumPoolSize = config.as("js7.thread-pools.io.maximum-pool-size")(StringAsIntOrUnlimited)
  //      .getOrElse(Int.MaxValue),
  //    queueSize = config.optionAs[Int]("js7.thread-pools.io.queue-size"))

  private def newThreadPoolExecutor(
    name: String,
    keepAlive: FiniteDuration,
    corePoolSize: Int = 0,
    maximumPoolSize: Int,
    queueSize: Option[Int])
  : ThreadPoolExecutor =
    new ThreadPoolExecutor(
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

  private def myThreadFactory(name: String): ThreadFactory =
    runnable => {
      val thread = new Thread(runnable)
      thread.setName(s"$name ${thread.threadId}")
      thread.setDaemon(true)  // Do it like Monix and Akka
      thread
    }

  java8Polyfill()
}
