package js7.common.system

import cats.effect.{Resource, Sync}
import com.typesafe.config.Config
import java.lang.Thread.currentThread
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.As
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichMonixResource
import js7.base.system.Java8Polyfill.*
import js7.base.thread.ThreadPoolsBase.{newBlockingExecutor, newBlockingNonVirtualExecutor}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.Closer
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.common.system.startup.Halt.haltJava
import monix.eval.Task
import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.atomic.AtomicInt
import monix.execution.schedulers.{ExecutorScheduler, SchedulerService}
import monix.execution.{ExecutionModel, Features, Scheduler, UncaughtExceptionReporter}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal


/**
  * @author Joacim Zschimmer
  */
object ThreadPools
{
  private val logger = Logger[this.type]

  private[system] val ThreadCount = As[String, Int] {
    case s if s.last == 'x' => (sys.runtime.availableProcessors * s.dropRight(1).toDouble).ceil.toInt
    case o => o.toInt
  }

  private val uncaughtExceptionReporter: UncaughtExceptionReporter = { throwable =>
    def msg = s"Uncaught exception in thread ${currentThread.threadId} '${currentThread.getName}': ${throwable.toStringWithCauses}"
    throwable match {
      case _: akka.stream.StreamTcpException | _: akka.http.scaladsl.model.EntityStreamException =>
        // TODO Not sure how to handle or ignore an unexpectedly closed connection while reading a stream.
        // "Entity stream truncation. The HTTP parser was receiving an entity when the underlying connection was closed unexpectedly."
        // Maybe, letting the thread die is normal Akka behaviour, and the original Akka thread pool does not log this ???
        logger.warn(msg, throwable.nullIfNoStackTrace)

      case NonFatal(_) =>
        logger.error(msg, throwable.nullIfNoStackTrace)

      case throwable: OutOfMemoryError =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        // Writes to stderr:
        UncaughtExceptionReporter.default.reportFailure(throwable)
        haltJava(s"💥 HALT DUE TO $throwable (heap size is ${toKiBGiB(sys.runtime.maxMemory)})",
          restart = true)

      case throwable =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        // Writes to stderr:
        UncaughtExceptionReporter.default.reportFailure(throwable)
    }
  }

  def unlimitedSchedulerResource[F[_]](name: String, config: Config)(implicit F: Sync[F])
  : Resource[F, Scheduler] =
    schedulerServiceToResource(F.delay(newUnlimitedScheduler(name, config)))
      .map(CorrelId.enableScheduler)

  private def newUnlimitedScheduler(name: String, config: Config): SchedulerService =
    ExecutorScheduler(
      newBlockingExecutor(config, name),
      uncaughtExceptionReporter, SynchronousExecution, Features.empty)

  def newUnlimitedNonVirtualScheduler(name: String): SchedulerService =
    ExecutorScheduler(
      newBlockingNonVirtualExecutor(name),
      uncaughtExceptionReporter, SynchronousExecution, Features.empty)

  private val nextNumber = AtomicInt(0)

  def ownThreadPoolResource[A](name: String, config: Config)
    (resource: Scheduler => Resource[Task, A])
  : Resource[Task, A] =
    for {
      ownScheduler <- standardSchedulerResource[Task](name, config)
      a <- resource(ownScheduler).executeOn(ownScheduler)
    } yield a

  def standardSchedulerResource[F[_]](name: String, config: Config, orCommon: Option[Scheduler])
    (implicit F: Sync[F])
  : Resource[F, Scheduler] =
    orCommon
      .match_ {
        case Some(scheduler) =>
          Resource.pure[F, Scheduler](CorrelId.enableScheduler(scheduler))
        case None =>
          standardSchedulerResource(name, config)
      }

  // May require an outer Scheduler (for example, global).
  def standardSchedulerResource[F[_]](name: String, config: Config)
    (implicit F: Sync[F])
  : Resource[F, Scheduler] =
    Resource
      .fromAutoCloseable(F.delay(new Closer))
      .map(newStandardScheduler(name, config, _))

  // Requires an outer Scheduler (global).
  def schedulerServiceToResource[F[_]](scheduler: F[SchedulerService])
    (implicit F: Sync[F])
  : Resource[F, SchedulerService] =
    Resource.make(
      acquire = scheduler)(
      release = o => F.delay(o.shutdown()))

  def newStandardScheduler(name: String, config: Config, closer: Closer): Scheduler = {
    val nr = nextNumber.incrementAndGet()
    val myName = if (isTest && nr > 1) s"$name-#$nr" else name
    val shutdownTimeout = config.getDuration("js7.thread-pools.standard.shutdown-timeout").toFiniteDuration
    val parallelism = config.as("js7.thread-pools.standard.parallelism")(ThreadCount)
    val maxThreads = config.getInt("js7.thread-pools.standard.maximum")
    logger.debug(s"newStandardScheduler $myName parallelism=$parallelism maxThreads=$maxThreads")

    val scheduler = ExecutorScheduler.forkJoinDynamic(myName,
      parallelism = parallelism,
      maxThreads = maxThreads,
      daemonic = true,
      reporter = uncaughtExceptionReporter,
      ExecutionModel.Default)

    closer.onClose {
      shutdownThreadPool(scheduler, myName, shutdownTimeout)
    }

    CorrelId.enableScheduler(scheduler)
  }

  private def shutdownThreadPool(
    scheduler: ExecutorScheduler,
    name: String,
    shutdownTimeout: FiniteDuration)
  : Unit = {
    val prefix = s"Scheduler($name)"
    logger.debugCall(s"$prefix.shutdown", "") {
      scheduler.shutdown()
      if (shutdownTimeout.isPositive) {
        logger.debug(s"$prefix.awaitTermination(${shutdownTimeout.pretty}) ...")
        if (!scheduler.awaitTermination(shutdownTimeout)) {
          logger.whenDebugEnabled {
            logger.debug(s"$prefix.awaitTermination(${shutdownTimeout.pretty}) timed out")
            Thread.getAllStackTraces.asScala
              .filter(_._1.getName startsWith name)
              .toSeq.sortBy(_._1.threadId)
              .foreach { case (thread, stacktrace) =>
                logger.debug(s"Thread #${thread.threadId} ${thread.getName} ⏎" +
                  stacktrace.map(o => s"\n  $o").mkString)
              }
          }
        } else {
          logger.debug("awaitTermination() finished")
        }
      }
    }
  }

  java8Polyfill()
}
