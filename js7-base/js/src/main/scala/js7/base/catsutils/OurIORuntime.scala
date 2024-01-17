package js7.base.catsutils

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{Resource, Sync}
import java.lang.Thread.currentThread
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.util.control.NonFatal

object OurIORuntime:

  // Lazy, to allow proper initialisation of logging
  private lazy val logger = Logger[this.type]

  // TODO How to shutdown IORuntime? Should we?
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
  def resource[F[_]](using F: Sync[F]): Resource[F, IORuntime] =
    Resource.eval(F.delay(ioRuntime))

  private def uncaughtExceptionReporter(throwable: Throwable) =
    def msg = s"Uncaught exception in thread ${currentThread.threadId} '${
      currentThread.getName}': ${throwable.toStringWithCauses}"

    throwable match
      case NonFatal(_) =>
        logger.error(msg, throwable.nullIfNoStackTrace)

      case throwable =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        throwable.printStackTrace(System.err)
