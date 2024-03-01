package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import cats.effect.{Resource, Sync}
import com.typesafe.config.{Config, ConfigFactory}
import java.lang.Thread.currentThread
import java.util.concurrent.ConcurrentHashMap
import js7.base.catsutils.CatsEffectExtensions.defer
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.system.Java8Polyfill.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.util.control.NonFatal

object OwnIORuntime:

  // Lazy, to allow proper initialisation of logging first
  private lazy val logger = Logger[this.type]
  private val usedNames = new ConcurrentHashMap[String, Int]

  def resource[F[_]](
    name: String,
    config: Config = ConfigFactory.empty,
    shutdownHooks: Seq[() => Unit] = Nil)
    (using F: Sync[F])
  : Resource[F, IORuntime] =
    val indexedName = toIndexedName(name)
    val resource = resource2[F](indexedName, config, shutdownHooks)
    Resource.defer:
      // Do not log for the initial IORuntime, before logging has been initialized.
      if Logger.isInitialized then
        logger.traceResource(s"$indexedName Resource[,IORuntime]"):
          resource
      else
        resource

  private def resource2[F[_]](
    name: String,
    config: Config = ConfigFactory.empty,
    shutdownHooks: Seq[() => Unit] = Nil)
    (using F: Sync[F])
  : Resource[F, IORuntime] =
    val computePrefix = s"$name-compute"
    val computeBlockerPrefix = s"$computePrefix-blocker"
    val blockingPrefix = s"$name-blocking"
    for
      pair <- Resource.eval(F.delay:
        IORuntime.createWorkStealingComputeThreadPool(
          threadPrefix = computePrefix,
          blockerThreadPrefix = computeBlockerPrefix,
          reportFailure = reportFailure))
      (compute, shutdownCompute) = pair
      _ <- Resource.onFinalize(F.delay(shutdownCompute()))

      pair <- Resource.eval(F.delay:
        IORuntime.createDefaultBlockingExecutionContext(threadPrefix = blockingPrefix))
      (blockingEC, shutdownBlocking) = pair
      _ <- Resource.onFinalize(F.delay(shutdownBlocking()))
      ioRuntime <- Resource.pure:
        val builder = IORuntime.builder()
          .setCompute(compute, shutdownCompute)
          .setBlocking(blockingEC, shutdownBlocking)
          .setFailureReporter(reportFailure)
        for hook <- shutdownHooks do builder.addShutdownHook(hook)
        builder.build()
      _ <- OwnIORuntimeRegister.register(compute, ioRuntime)
    yield
      ioRuntime

  /** Testing only: Differentiate a repeated name with an index. */
  private def toIndexedName(name: String): String =
    usedNames.computeIfAbsent(name, _ => 0)
    usedNames.compute(name, (_, i) => i + 1) match
      case 1 => name
      case i => s"$name-#$i"

  private def reportFailure(throwable: Throwable): Unit =
    def msg = s"Uncaught exception in thread ${currentThread.threadId} '${
      currentThread.getName
    }': ${throwable.toStringWithCauses}"

    throwable match
      //case _: pekko.stream.StreamTcpException | _: org.apache.pekko.http.scaladsl.model.EntityStreamException =>
      //  // TODO Not sure how to handle or ignore an unexpectedly closed connection while reading a stream.
      //  // "Entity stream truncation. The HTTP parser was receiving an entity when the underlying connection was closed unexpectedly."
      //  // Maybe, letting the thread die is normal Pekko behaviour, and the original Pekko thread pool does not log this ???
      //  logger.warn(msg, throwable.nullIfNoStackTrace)

      case NonFatal(_) =>
        logger.error(msg, throwable.nullIfNoStackTrace)

      // TODO Not active, because haltJava is placed in js7-common package
      //case throwable: OutOfMemoryError =>
      //  logger.error(msg, throwable.nullIfNoStackTrace)
      //  throwable.printStackTrace(System.err)
      //  haltJava(s"💥 HALT DUE TO $throwable (heap size is ${toKiBGiB(sys.runtime.maxMemory)})",
      //    restart = true)

      case throwable =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        throwable.printStackTrace(System.err)

  java8Polyfill()
