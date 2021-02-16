package js7.base.thread

import java.lang.Thread.currentThread
import java.util.concurrent.{Executor, ThreadPoolExecutor}
import js7.base.log.Logger
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.IOExecutor._
import js7.base.thread.ThreadPoolsBase.newUnlimitedThreadPool
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

/**
  * For `ioFuture` which starts a blocking (I/O) `Future` in a (normally unlimited) thread pool.
  * @author Joacim Zschimmer
  */
final class IOExecutor(threadPool: ThreadPoolExecutor) extends Executor
{
  def this(name: String, keepAlive: FiniteDuration = 10.s) =
    this(newUnlimitedThreadPool(name = name, keepAlive = keepAlive))

  private val myExecutionContext = ExecutionContext.fromExecutor(
    threadPool,
    t => logger.error(t.toStringWithCauses, t))

  lazy val scheduler = Scheduler(myExecutionContext, uncaughtExceptionReporter, SynchronousExecution)

  def execute(runnable: Runnable) = myExecutionContext.execute(runnable)

  def shutdown(): Unit =
    threadPool.shutdown()

  implicit def executionContext: ExecutionContext = myExecutionContext
}

object IOExecutor
{
  private val logger = Logger[this.type]
  val globalIOX = new IOExecutor(name = "JS7 global I/O")

  object Implicits {
    implicit val globalIOX = IOExecutor.globalIOX
  }

  def ioFuture[A](body: => A)(implicit iox: IOExecutor): Future[A] =
    try
      promiseFuture[A] { promise =>
        iox execute { () =>
          promise.complete(Try {
            body
          })
        }
      }
    catch {
      case NonFatal(t) => Future.failed(t)
    }

  private val uncaughtExceptionReporter: UncaughtExceptionReporter = { throwable =>
    def msg = s"Uncaught exception in thread ${currentThread.getId} '${currentThread.getName}': ${throwable.toStringWithCauses}"
    throwable match {
      case NonFatal(_) =>
        logger.error(msg, throwable.nullIfNoStackTrace)

      case throwable =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        // Writes to stderr:
        UncaughtExceptionReporter.default.reportFailure(throwable)
    }
  }
}
