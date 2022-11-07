package js7.base.thread

import cats.effect.{Resource, Sync}
import com.typesafe.config.Config
import java.lang.Thread.currentThread
import java.util.concurrent.Executor
import js7.base.log.{CorrelId, Logger}
import js7.base.system.Java8Polyfill.*
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.IOExecutor.*
import js7.base.thread.ThreadPoolsBase.newBlockingExecutor
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import monix.eval.{Task, TaskLike}
import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

/**
  * For `ioFuture` which starts a blocking (I/O) `Future` in a (normally unlimited) thread pool.
  * @author Joacim Zschimmer
  */
final class IOExecutor(executor: Executor) extends Executor
{
  implicit val executionContext = ExecutionContext.fromExecutor(
    executor,
    t => logger.error(t.toStringWithCauses, t))

  lazy val scheduler = CorrelId.enableScheduler(
    Scheduler(executionContext, uncaughtExceptionReporter, SynchronousExecution))

  def execute(runnable: Runnable) = executionContext.execute(runnable)

  def apply[F[_], A](task: F[A])(implicit F: TaskLike[F]): Task[A] =
    F(task) executeOn scheduler
}

object IOExecutor
{
  private val logger = Logger[this.type]
  lazy val globalIOX = new IOExecutor(
    newBlockingExecutor(name = "JS7 global I/O", keepAlive = 10.s))

  object Implicits {
    implicit lazy val globalIOX: IOExecutor = IOExecutor.globalIOX
  }

  def resource[F[_]](config: Config, name: String)(implicit F: Sync[F]): Resource[F, IOExecutor] =
    Resource
      .make(
        acquire = F.delay(newBlockingExecutor(config, name)))(
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

  private[thread] val uncaughtExceptionReporter: UncaughtExceptionReporter = { throwable =>
    def msg = s"Uncaught exception in thread ${currentThread.threadId} '${currentThread.getName}': ${throwable.toStringWithCauses}"
    throwable match {
      case NonFatal(_) =>
        logger.error(msg, throwable.nullIfNoStackTrace)

      case throwable =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        // Writes to stderr:
        UncaughtExceptionReporter.default.reportFailure(throwable)
    }
  }

  java8Polyfill()
}
