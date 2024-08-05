package js7.base.thread

import cats.effect.{IO, Resource, Sync}
import java.lang.Thread.currentThread
import java.util.concurrent.{Executor, ExecutorService}
import js7.base.catsutils.CatsEffectExtensions.{blockingOn, defer}
import js7.base.catsutils.Environment.environment
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.system.Java17Polyfill.*
import js7.base.thread.IOExecutor.*
import js7.base.thread.ThreadPoolsBase.newBlockingExecutorService
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.control.NonFatal

final class IOExecutor(private[IOExecutor] val executor: ExecutorService, name: String)
  extends Executor:

  given executionContext: ExecutionContextExecutor =
    new ExecutionContextExecutor:
      private val delegate = ExecutionContext.fromExecutor(executor, reportException)
      export delegate.*
      override def toString = name

  def execute(runnable: Runnable): Unit =
    executionContext.execute(runnable)

  private def reportException(throwable: Throwable): Unit =
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
  private lazy val logger = Logger[this.type]

  val globalName = "JS7 global I/O"

  lazy val globalIOX: IOExecutor =
    new IOExecutor(
      newBlockingExecutorService(globalName, virtual = true),
      globalName)

  object Implicits:
    implicit lazy val globalIOX: IOExecutor = IOExecutor.globalIOX

  def resource[F[_]](name: String)(using F: Sync[F]): Resource[F, IOExecutor] =
    Resource.defer:
      // Don't log here, because it may be called before Logger initialization,
      // when called by OurIORuntime!
      logger.debugResource:
        Resource
          .make(
            acquire = F.delay(newBlockingExecutorService(name, virtual = true)))(
            release = executor => F.delay:
              logger.debug(s"shutdown $executor")
              executor.shutdown())
          .map(new IOExecutor(_, name))

  private[thread] def blocking[A](body: => A)(using iox: IOExecutor): IO[A] =
    IO.blockingOn(iox.executionContext):
      body

  private[thread] def interruptible[A](body: => A)(using iox: IOExecutor): IO[A] =
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
        Some(IO(future.cancel(true)))

  object env:
    /** Like IO blocking, but executes in a virtual thread. */
    def virtualThread[A](body: => A): IO[A] =
      environment[IOExecutor].flatMap: iox =>
        IOExecutor.blocking(body)(using iox)

    /** Like IO interruptible, but executes in a virtual thread. */
    def interruptibleVirtualThread[A](body: => A): IO[A] =
      environment[IOExecutor].flatMap: iox =>
        IOExecutor.interruptible(body)(using iox)

  java17Polyfill()
