package js7.base.thread

import cats.effect.{Resource, Sync}
import com.typesafe.config.Config
import java.lang.Thread.currentThread
import java.util.concurrent.{Executor, ThreadPoolExecutor}
import js7.base.log.{CorrelId, Logger}
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.IOExecutor._
import js7.base.thread.ThreadPoolsBase.newUnlimitedThreadPool
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import monix.eval.Task
import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

/**
  * For `ioFuture` which starts a blocking (I/O) `Future` in a (normally unlimited) thread pool.
  * @author Joacim Zschimmer
  */
final class IOExecutor(threadPool: ThreadPoolExecutor) extends Executor
{
  private val myExecutionContext = ExecutionContext.fromExecutor(
    threadPool,
    t => logger.error(t.toStringWithCauses, t))

  lazy val scheduler = CorrelId.enableScheduler(
    Scheduler(myExecutionContext, uncaughtExceptionReporter, SynchronousExecution))

  def execute(runnable: Runnable) = myExecutionContext.execute(runnable)

  implicit def executionContext: ExecutionContext = myExecutionContext
}

object IOExecutor
{
  private val logger = Logger[this.type]
  val globalIOX = new IOExecutor(newUnlimitedThreadPool(name = "JS7 global I/O", keepAlive = 10.s))

  object Implicits {
    implicit val globalIOX = IOExecutor.globalIOX
  }

  def resource[F[_]](config: Config, name: String)(implicit F: Sync[F]): Resource[F, IOExecutor] =
    Resource
      .make(
        acquire = F.delay(newUnlimitedThreadPool(config, name)))(
        release = o => F.delay(o.shutdown()))
      .map(new IOExecutor(_))

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

  def ioTask[A](body: => A)(implicit iox: IOExecutor): Task[A] =
    Task(body) executeOn iox.scheduler

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
