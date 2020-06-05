package js7.common.scalautil

import com.typesafe.config.Config
import java.util.concurrent.{Executor, LinkedBlockingQueue, SynchronousQueue, ThreadFactory, ThreadPoolExecutor}
import js7.base.convert.As.StringAsIntOrUnlimited
import js7.base.utils.ScalaUtils.RichThrowable
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.scalautil.Futures.promiseFuture
import js7.common.scalautil.IOExecutor.{logger, _}
import js7.common.time.JavaTimeConverters._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.Try
import scala.util.control.NonFatal

/**
  * For `ioFuture` which starts a blocking (I/O) `Future` in a (normally unlimited) thread pool.
  * @author Joacim Zschimmer
  */
final class IOExecutor(_executionContext: ExecutionContext) extends Executor
{
  def this(threadPool: ThreadPoolExecutor) = this(
    ExecutionContext.fromExecutor(
      threadPool,
      t => logger.error(t.toStringWithCauses, t)))

  def this(keepAlive: FiniteDuration, name: String) = this(newThreadPoolExecutor(keepAlive, name = name))

  def execute(runnable: Runnable) = _executionContext.execute(runnable)

  implicit def executionContext: ExecutionContext = _executionContext
}

object IOExecutor
{
  private val logger = Logger(getClass)

  object Implicits {
    implicit val globalIOX = new IOExecutor(60.seconds, name = "JS7")
  }

  def newThreadPoolExecutor(config: Config, name: String): ThreadPoolExecutor =
    newThreadPoolExecutor(
      keepAlive = config.getDuration("js7.thread-pools.io.keep-alive").toFiniteDuration,
      minimum   = config.getInt     ("js7.thread-pools.io.minimum"),
      maximum   = config.as         ("js7.thread-pools.io.maximum")(StringAsIntOrUnlimited),
      name      = name)

  def newThreadPoolExecutor(keepAlive: FiniteDuration = 60.seconds, minimum: Int = 0, maximum: Option[Int] = None, name: String): ThreadPoolExecutor =
    new ThreadPoolExecutor(minimum, maximum getOrElse Int.MaxValue, keepAlive.toMillis, MILLISECONDS,
      if (maximum.isEmpty) new SynchronousQueue[Runnable] else new LinkedBlockingQueue[Runnable],
      myThreadFactory(name)) //with NamedRunnable.RenamesThread

  private def myThreadFactory(name: String): ThreadFactory = runnable => {
    val thread = new Thread(runnable)
    thread.setName(s"$name I/O ${thread.getId}")
    thread.setDaemon(true)  // Do it like Monix and Akka
    thread
  }

  def ioFuture[A](body: => A)(implicit iox: IOExecutor): Future[A] =
    try
      promiseFuture[A] { p =>
        iox execute { () =>
          p.complete(Try {
            blocking {
              body
            }
          })
        }
      }
    catch {
      case NonFatal(t) => Future.failed(t)
    }
}
