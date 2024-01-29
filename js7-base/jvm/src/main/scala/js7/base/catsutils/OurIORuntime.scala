package js7.base.catsutils

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{Resource, Sync}
import com.typesafe.config.Config
import java.lang.Thread.currentThread
import js7.base.log.Logger
import js7.base.system.Java8Polyfill.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.util.control.NonFatal

object OurIORuntime:

  // Lazy, to allow proper initialisation of logging
  private lazy val logger = Logger[this.type]

  private var _ioRuntime: IORuntime =
    IORuntime.builder()
      .setFailureReporter(uncaughtExceptionReporter)
      .addShutdownHook: () =>
        _ioRuntime = null
      .build()

  def ioRuntime: IORuntime =
    _ioRuntime

  given IORuntime = ioRuntime

  def scheduler: Scheduler =
    ioRuntime.scheduler

  /** For now, we return always the same global IORuntime and never shuts it down*/
  def resource[F[_]](name: String, config: Config)(using F: Sync[F]): Resource[F, IORuntime] =
    Resource.eval(F.delay(ioRuntime))
    // How to shutdown a self-built IORuntime ?

  private def uncaughtExceptionReporter(throwable: Throwable) =
    def msg = s"Uncaught exception in thread ${currentThread.threadId} '${
      currentThread.getName}': ${throwable.toStringWithCauses}"

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