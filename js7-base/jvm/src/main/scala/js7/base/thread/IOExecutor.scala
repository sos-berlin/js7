package js7.base.thread

import cats.effect.implicits.asyncOps
import cats.effect.kernel.Async
import cats.effect.syntax.async.*
import cats.effect.{IO, Resource, Sync}
import com.typesafe.config.Config
import java.lang.Thread.currentThread
import java.util.concurrent.{Executor, ExecutorService}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.system.Java8Polyfill.*
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.IOExecutor.*
import js7.base.thread.ThreadPoolsBase.newBlockingExecutor
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Try
import scala.util.control.NonFatal

/**
  * For `ioFuture` which starts a blocking (I/O) `Future` in a (normally unlimited) thread pool.
  * @author Joacim Zschimmer
  */
final class IOExecutor(executor: Executor, name: String) extends Executor:

  given executionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(executor, reportException)

  private def reportException(throwable: Throwable) =
    def msg = "Uncaught exception in thread " +
      s"${currentThread.threadId} '${currentThread.getName}': ${throwable.toStringWithCauses}"

    throwable match
      case throwable: java.util.concurrent.RejectedExecutionException =>
        val isShuttDown = executor match
          case executor: ExecutorService => executor.isShutdown
          case _ => false
        if isShuttDown then
          logger.error(s"'$name' ${executor.getClass.simpleScalaName} has been shut down: $msg",
            throwable.nullIfNoStackTrace)
        else
          logger.error(msg, throwable.nullIfNoStackTrace)

      case _ =>
        logger.error(msg, throwable.nullIfNoStackTrace)

  def execute(runnable: Runnable) =
    executionContext.execute(runnable)

  def apply[F[_], A](body: F[A])(using F: Async[F], src: sourcecode.FullName): F[A] =
    // logger.traceF lets Monix check for cancellation, too (but why?)
    // Without it, the body seems be executed after a cancellation,
    // despite executor has already been terminated (observed with DirectoryWatch).
    logger.traceF(s"${src.value} --> IOExecutor($name).apply")(
      body.evalOn(executionContext))


object IOExecutor:
  private val logger = Logger[this.type]

  lazy val globalIOX: IOExecutor =
    val name = "JS7 global I/O"
    new IOExecutor(
      newBlockingExecutor(name, keepAlive = 10.s),
      name)

  object Implicits:
    implicit lazy val globalIOX: IOExecutor = IOExecutor.globalIOX

  def resource[F[_]](config: Config, name: String)(implicit F: Sync[F]): Resource[F, IOExecutor] =
    logger.traceResource(
      Resource
        .make(
          acquire = F.delay(newBlockingExecutor(config, name)))(
          release = executor => F.delay {
            logger.debug(s"shutdown $executor")
            executor.shutdown()
          })
        .map(new IOExecutor(_, name)))

  def ioFuture[A](body: => A)(implicit iox: IOExecutor): Future[A] =
    try
      promiseFuture[A] { promise =>
        iox execute { () =>
          promise.complete(Try {
            body
          })
        }
      }
    catch
      case NonFatal(t) => Future.failed(t)

  java8Polyfill()
