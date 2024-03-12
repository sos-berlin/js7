package js7.common.system

import cats.effect.{Resource, Sync}
import com.typesafe.config.Config
import java.lang.Thread.currentThread
import java.util.concurrent.ExecutorService
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.system.Java17Polyfill.*
import js7.base.thread.ThreadPoolsBase.{labeledExecutionContextExecutorService, newBlockingExecutorService, newBlockingNonVirtualExecutor}
import js7.base.time.ScalaTime.*
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.system.startup.Halt.haltJava
import org.apache.pekko
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object ThreadPools:

  private val logger = Logger[this.type]

  private def reportUncaughtException(throwable: Throwable): Unit =
    def msg = s"Uncaught exception in thread ${currentThread.threadId} '${currentThread.getName}': ${throwable.toStringWithCauses}"
    throwable match
      case _: pekko.stream.StreamTcpException | _: org.apache.pekko.http.scaladsl.model.EntityStreamException =>
        // TODO Not sure how to handle or ignore an unexpectedly closed connection while reading a stream.
        // "Entity stream truncation. The HTTP parser was receiving an entity when the underlying connection was closed unexpectedly."
        // Maybe, letting the thread die is normal Pekko behaviour, and the original Pekko thread pool does not log this ???
        logger.warn(msg, throwable.nullIfNoStackTrace)

      case NonFatal(_) =>
        logger.error(msg, throwable.nullIfNoStackTrace)

      case throwable: OutOfMemoryError =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        // Writes to stderr:
        throwable.printStackTrace(System.err)
        haltJava(s"💥 HALT DUE TO $throwable (heap size is ${toKiBGiB(sys.runtime.maxMemory)})",
          restart = true)

      case throwable =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        // Writes to stderr:
        throwable.printStackTrace(System.err)

  def unlimitedExecutionContextResource[F[_]](
    name: String, config: Config, virtual: Boolean = false)
    (using F: Sync[F])
  : Resource[F, ExecutionContext] =
      schedulerServiceToResource(F.delay:
        newUnlimitedExecutionContext(name, config, virtual = virtual))

  private def newUnlimitedExecutionContext(name: String, config: Config, virtual: Boolean = false)
  : ExecutionContextExecutorService =
    labeledExecutionContextExecutorService(name):
      ExecutionContext.fromExecutorService(
        newBlockingExecutorService(name, config, virtual = virtual),
        reportUncaughtException)

  def newUnlimitedNonVirtualExecutionContext(name: String): ExecutionContextExecutorService =
    labeledExecutionContextExecutorService(name):
      ExecutionContext.fromExecutorService(
        newBlockingNonVirtualExecutor(name),
        reportUncaughtException)

  // Requires an outer Scheduler (global).
  def schedulerServiceToResource[F[_]](ec: F[ExecutionContextExecutorService])
    (implicit F: Sync[F])
  : Resource[F, ExecutionContextExecutorService] =
    Resource.make(
      acquire = ec)(
      release = ec => F.delay(ec.shutdown()))

  private def shutdownExecutorService(
    executorService: ExecutorService,
    name: String,
    shutdownTimeout: FiniteDuration)
  : Unit =
    val prefix = s"Scheduler($name)"
    logger.debugCall(s"$prefix.shutdown", ""):
      executorService.shutdown()
      if shutdownTimeout.isPositive then
        logger.debug(s"$prefix.awaitTermination(${shutdownTimeout.pretty}) ...")
        if !executorService.awaitTermination(shutdownTimeout.toMillis, MILLISECONDS) then
          logger.whenDebugEnabled:
            logger.debug(s"$prefix.awaitTermination(${shutdownTimeout.pretty}) timed out")
            Thread.getAllStackTraces.asScala
              .filter(_._1.getName startsWith name)
              .toSeq.sortBy(_._1.threadId)
              .foreach { case (thread, stacktrace) =>
                logger.debug(s"Thread #${thread.threadId} ${thread.getName} ⏎" +
                  stacktrace.map(o => s"\n  $o").mkString)
              }
        else
          logger.debug("awaitTermination() finished")

  java17Polyfill()
