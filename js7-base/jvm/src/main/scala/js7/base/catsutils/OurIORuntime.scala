package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import cats.effect.{Resource, Sync, SyncIO}
import com.typesafe.config.Config
import java.lang.Thread.currentThread
import java.util.concurrent.{ConcurrentHashMap, Executor}
import js7.base.catsutils.CatsEffectExtensions.defer
import js7.base.configutils.Configs.{ConvertibleConfig, HoconStringInterpolator}
import js7.base.convert.As
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.system.Java17Polyfill.*
import js7.base.system.startup.Halt
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.{isTest, isTestParallel}
import js7.base.utils.UseDefault.getOrElse
import js7.base.utils.{Tests, UseDefault}
import Halt.haltJava
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

object OurIORuntime:

  // Lazy, to allow proper initialisation of logging first
  private lazy val logger = Logger[this.type]

  val commonThreadPrefix = "JS7"

  val useCommonIORuntime: Boolean =
    isTest && (
      sys.props.contains("js7.test.commonIORuntime") || sys.props.contains("test.speed"))

  final lazy val commonIORuntime: IORuntime =
    ownResource[SyncIO](
      commonThreadPrefix,
      config"""js7.thread-pools.compute.threads = 1/1"""
    ).allocated.map(_._1)
      .unsafeRunSync()

  private val usedNames = new ConcurrentHashMap[String, Int]

  def resource[F[_]](
    label: String,
    config: Config,
    threads: Int | UseDefault = UseDefault,
    shutdownHooks: Seq[() => Unit] = Nil)
    (using F: Sync[F])
  : Resource[F, IORuntime] =
    if useCommonIORuntime then
      Resource.pure(commonIORuntime)
    else
      ownResource[F](label, config, threads, shutdownHooks)

  def ownResource[F[_]](
    label: String,
    config: Config,
    threads: Int | UseDefault = UseDefault,
    shutdownHooks: Seq[() => Unit] = Nil,
    computeExecutor: Option[Executor] = None)
    (using F: Sync[F])
  : Resource[F, IORuntime] =
    val indexedLabel = toIndexedName(label)
    val myThreads = threads.getOrElse:
      if isTestParallel then
        2 // Room for some other concurrently running tests and "internal" nodes
      else
        2 max config.as("js7.thread-pools.compute.threads")(ThreadCount)
    val resource = resource2[F](indexedLabel, threads = myThreads, shutdownHooks, computeExecutor)
    Resource.defer:
      // Do not log for the initial IORuntime, before logging has been initialized.
      if Logger.isInitialized then
        logger.traceResource(s"$indexedLabel Resource[,IORuntime]"):
          resource
      else
        resource

  private def resource2[F[_]](
    label: String,
    threads: Int,
    shutdownHooks: Seq[() => Unit] = Nil,
    computeExecutor: Option[Executor])
    (using F: Sync[F])
  : Resource[F, IORuntime] =
    val computeLabel = s"$label compute"
    val computeBlockerLabel = s"$label compute-blocker"
    val blockingLabel = s"$label blocking"

    for
      pair <-
        Resource.eval(F.delay:
          computeExecutor match
            case Some(ec) =>
              toExecutionContext(ec) -> (() => ())
            case None =>
              IORuntime.createWorkStealingComputeThreadPool(
                threads = threads,
                threadPrefix = computeLabel,
                blockerThreadPrefix = computeBlockerLabel,
                reportFailure = reportFailure))
      (compute, shutdownCompute) = pair
      _ <- Resource.onFinalize(F.delay(shutdownCompute()))

      pair <- Resource.eval(F.delay:
        IORuntime.createDefaultBlockingExecutionContext(threadPrefix = blockingLabel))
      (blockingEC, shutdownBlocking) = pair
      _ <- Resource.onFinalize(F.delay(shutdownBlocking()))

      ioRuntime <- Resource.pure:
        val builder = IORuntime.builder()
          .setCompute(/*labeledExecutionContext(computeLabel)*/(compute), shutdownCompute)
          .setBlocking(/*labeledExecutionContext(blockingLabel)*/(blockingEC), shutdownBlocking)
          .setFailureReporter(reportFailure)
        for hook <- shutdownHooks do builder.addShutdownHook(hook)
        builder.build()
      _ <- OurIORuntimeRegister.register(compute, ioRuntime)
    yield
      ioRuntime

  /** Testing only: Differentiate a repeated name with an index. */
  private def toIndexedName(name: String): String =
    usedNames.computeIfAbsent(name, _ => 0)
    usedNames.compute(name, (_, i) => i + 1) match
      case 1 => name
      case i => s"$name-#$i"

  private def toExecutionContext(executor: Executor): ExecutionContext =
    executor match
      case ec: ExecutionContext => ec
      case _ =>
        new ExecutionContext:
          def execute(runnable: Runnable) = executor.execute(runnable)

          def reportFailure(t: Throwable) = OurIORuntime.reportFailure(t)

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
        // Different to Monix, Cats Effect seems to call reportFailure for some irrelevant exceptions
        logger.debug(msg, throwable.nullIfNoStackTrace)

      case throwable: OutOfMemoryError =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        throwable.printStackTrace(System.err)
        haltJava(s"💥 HALT DUE TO $throwable (heap size is ${toKiBGiB(sys.runtime.maxMemory)})",
          restart = true)

      case throwable =>
        logger.error(msg, throwable.nullIfNoStackTrace)
        throwable.printStackTrace(System.err)

  private[catsutils] val ThreadCount: As[String, Int] =
    val Fraction = "([0-9]+)/([0-9]+)".r
    {
      case Fraction(numerator, denominator) =>
        val m = denominator.toInt
        if m == 0 then throw
          new IllegalArgumentException("Zero denominator in ThreadCount configuration entry")
        (sys.runtime.availableProcessors * numerator.toDouble / denominator.toInt).ceil.toInt

      case o => o.toInt
    }

  java17Polyfill()
