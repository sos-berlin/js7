package js7.base.thread

import cats.effect.implicits.asyncOps
import cats.effect.{Async, IO, Resource, Sync}
import com.typesafe.config.Config
import java.lang.Thread.currentThread
import java.util.concurrent.{Executor, ExecutorService}
import js7.base.catsutils.CatsEffectExtensions.blockingOn
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger.syntax.*
import js7.base.log.Logger
import js7.base.system.Java8Polyfill.*
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.IOExecutor.*
import js7.base.thread.ThreadPoolsBase.newBlockingExecutorService
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Try
import scala.util.control.NonFatal

/**
  * For `ioFuture` which starts a blocking (I/O) `Future` in a (normally unlimited) thread pool.
  * @author Joacim Zschimmer
  */
final class IOExecutor(private[IOExecutor] val executor: ExecutorService, name: String)
  extends Executor:

  given executionContext: ExecutionContextExecutor =
    new ExecutionContextExecutor:
      private val delegate = ExecutionContext.fromExecutor(executor, reportException)
      export delegate.*
      override def toString = name

  def execute(runnable: Runnable) =
    executionContext.execute(runnable)

  def apply[F[_], A](body: F[A])(using F: Async[F], src: sourcecode.FullName): F[A] =
    // logger.traceF lets Monix check for cancellation, too (but why?)
    // Without it, the body seems be executed after a cancellation,
    // despite executor has already been terminated (observed with DirectoryWatch).
    logger.traceF(s"${src.value} --> IOExecutor($name).apply")(
      body.evalOn(executionContext))

  private def reportException(throwable: Throwable) =
    def msg = "Uncaught exception in thread " +
      s"${currentThread.threadId} '${currentThread.getName}': ${throwable.toStringWithCauses}"

    throwable match
      case throwable: java.util.concurrent.RejectedExecutionException =>
        if executor.isShutdown then
          logger.error(s"'$name' ${executor.getClass.simpleScalaName} has been shut down: $msg",
            throwable.nullIfNoStackTrace)
        else
          logger.error(msg, throwable.nullIfNoStackTrace)

      case _ =>
        logger.error(msg, throwable.nullIfNoStackTrace)


object IOExecutor:
  private val logger = Logger[this.type]

  lazy val globalIOX: IOExecutor =
    val name = "JS7 global I/O"
    new IOExecutor(
      newBlockingExecutorService(
        name,
        config"""
          js7.thread-pools.long-blocking.keep-alive = 60s
          js7.thread-pools.long-blocking.virtual = true
          """,
        virtual = true),
      name)

  object Implicits:
    implicit lazy val globalIOX: IOExecutor = IOExecutor.globalIOX

  def resource[F[_]](config: Config, name: String)(implicit F: Sync[F]): Resource[F, IOExecutor] =
    logger.traceResource(
      Resource
        .make(
          acquire = F.delay(newBlockingExecutorService(name, config, virtual = true)))(
          release = executor => F.delay {
            logger.debug(s"shutdown $executor")
            executor.shutdown()
          })
        .map(new IOExecutor(_, name)))

  def ioFuture[A](body: => A)(implicit iox: IOExecutor): Future[A] =
    try
      promiseFuture[A] { promise =>
        iox.execute { () =>
          promise.complete(Try {
            body
          })
        }
      }
    catch
      case NonFatal(t) => Future.failed(t)

  def blocking[A](body: => A)(using iox: IOExecutor): IO[A] =
    IO.blockingOn(iox.executionContext):
      body

  def interruptible[A](body: => A)(using iox: IOExecutor): IO[A] =
    IO.async: callback =>
      IO:
        val runnable: Runnable =
          () => callback:
            try
              val a = body
              Right(a)
            catch
              case NonFatal(t) => Left(t)
              case t: InterruptedException => Left(t) // What happens with t ???
            finally
              Thread.interrupted() // Clear the interrupted flag and ignore the late interruption (?)

        val future = iox.executor.submit(runnable)
        Some(IO:
          future.cancel(true))

  java8Polyfill()
