package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.base.convert.As.StringAsIntOrUnlimited
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.IOExecutor.{logger, _}
import com.sos.jobscheduler.common.time.ScalaTime.RichDuration
import com.typesafe.config.Config
import java.util.concurrent.{Executor, LinkedBlockingQueue, SynchronousQueue, ThreadFactory, ThreadPoolExecutor}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.Try
import scala.util.control.NonFatal

/**
  * For `ioFuture` which starts a blocking (I/O) `Future` in a (normally unlimited) thread pool.
  * @author Joacim Zschimmer
  */
final class IOExecutor(executionContext: ExecutionContext) extends Executor
{
  def this(threadPool: ThreadPoolExecutor) = this(
    ExecutionContext.fromExecutor(
      threadPool,
      t ⇒ logger.error(t.toStringWithCauses, t)))

  def this(keepAlive: FiniteDuration) = this(newThreadPoolExecutor(keepAlive))

  def execute(runnable: Runnable) = executionContext.execute(runnable)
}

object IOExecutor
{
  private val logger = Logger(getClass)

  def newThreadPoolExecutor(config: Config): ThreadPoolExecutor =
    newThreadPoolExecutor(
      keepAlive = config.getDuration("jobscheduler.io-thread-pool.keep-alive").toFiniteDuration,
      minimum   = config.getInt     ("jobscheduler.io-thread-pool.minimum"),
      maximum   = config.as         ("jobscheduler.io-thread-pool.maximum")(StringAsIntOrUnlimited))

  def newThreadPoolExecutor(keepAlive: FiniteDuration = 60.seconds, minimum: Int = 0, maximum: Option[Int] = None): ThreadPoolExecutor =
    new ThreadPoolExecutor(minimum, maximum getOrElse Int.MaxValue, keepAlive.toMillis, MILLISECONDS,
      if (maximum.isEmpty) new SynchronousQueue[Runnable] else new LinkedBlockingQueue[Runnable],
      MyThreadFactory) //with NamedRunnable.RenamesThread

  private val MyThreadFactory: ThreadFactory = runnable ⇒ {
    val thread = new Thread(runnable)
    thread.setName(s"JobScheduler I/O ${thread.getId}")
    thread.setDaemon(true)  // Do it like Monix and Akka
    thread
  }

  def ioFuture[A](body: ⇒ A)(implicit iox: IOExecutor): Future[A] =
    try
      promiseFuture[A] { p ⇒
        iox execute { () ⇒
          p.complete(Try {
            blocking {
              body
            }
          })
        }
      }
    catch {
      case NonFatal(t) ⇒ Future.failed(t)
    }
}
