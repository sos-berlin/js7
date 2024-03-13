package js7.base.log

import cats.effect.Resource.ExitCase
import cats.effect.implicits.monadCancelOps
import cats.effect.{IO, Outcome, Resource, Sync, SyncIO}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import com.typesafe.scalalogging.Logger as ScalaLogger
import fs2.Stream
import izumi.reflect.Tag
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.log.Log4j.{initialize, shutdown}
import js7.base.log.Slf4jUtils.syntax.*
import js7.base.problem.Problem
import js7.base.system.startup.StartUp
import js7.base.time.ScalaTime.{DurationRichLong, RichDeadline, RichDuration}
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichJavaClass, RichThrowable}
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.{Once, Tests}
import org.slf4j.{LoggerFactory, Marker, MarkerFactory}
import scala.annotation.unused
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.reflect.ClassTag

object Logger extends AdHocLogger:

  type Underlying = ScalaLogger

  private val ifNotInitialized = new Once

  Slf4jUtils.initialize()

  lazy val empty: ScalaLogger =
    warnIfNotInitialized(classOf[this.type])
    ScalaLogger(org.slf4j.helpers.NOPLogger.NOP_LOGGER)

  def resource[F[_]](name: String)(using F: Sync[F]): Resource[F, Unit] =
    Resource.make(
      acquire = F.delay:
        initialize(name))(
      release = _ => F.delay:
        Log4j.shutdown())

  def initialize(name: String, suppressInfo: Boolean = false): Unit =
    ifNotInitialized:
      Log4j.initialize(name)
      if !suppressInfo then
        ScalaLogger[this.type].info(StartUp.startUpLine(name))
      Tests.log()

  def initializeOnly(name: String): Unit =
    ifNotInitialized:
      Log4j.initialize(name)

  /** Don't initialize but mark as initialized.
   * Use this when logging has been initialized by some outer software (like JOC). */
  def dontInitialize(): Unit =
    ifNotInitialized:
      Tests.log()

  def isInitialized: Boolean =
    ifNotInitialized.isInitialized

  //val Timing: Marker = MarkerFactory.getMarker("Timing")
  //val Event: Marker = MarkerFactory.getMarker("Event")
  val Actor: Marker = MarkerFactory.getMarker("Actor")
  val Java: Marker = MarkerFactory.getMarker("Java")
  //val Repo: Marker = MarkerFactory.getMarker("Repo")
  val SignatureVerified: Marker = MarkerFactory.getMarker("SignatureVerified")

  def apply[A: ClassTag]: ScalaLogger =
    apply(implicitClass[A])

  def apply(c: Class[?]): ScalaLogger =
    warnIfNotInitialized(c)
    ScalaLogger(normalizeClassName(c))

  def apply(name: String): ScalaLogger =
    warnIfNotInitialized(null, name)
    ScalaLogger(name)

  def withPrefix[A: ClassTag](prefix: String): ScalaLogger =
    withPrefix(implicitClass[A], prefix)

  def withPrefix(c: Class[?], prefix: String): ScalaLogger =
    if prefix.isEmpty then
      apply(c)
    else
      warnIfNotInitialized(c, prefix)
      ScalaLogger(new ConvertingLogger.Prefixed(
        prefix,
        LoggerFactory.getLogger(normalizeClassName(c))))

  private def warnIfNotInitialized(cls: Class[?] | Null, prefix: String = "") =
    if !ifNotInitialized.isInitializing then
      val classArg = if cls == null then "" else s"[${cls.scalaName}]"
      val prefixArg = prefix.nonEmpty ?? s"(\"$prefix\")"
      val t = new Exception("")
      ScalaLogger[this.type]
        .warn(s"Logger$classArg$prefixArg before Logger has been initialized", t)

  /** Removes '$' from Scala's companion object class. */
  private def normalizeClassName(c: Class[?]): String =
    c.getName stripSuffix "$"

  object ops:
    implicit final class RichScalaLogger(private val logger: ScalaLogger) extends AnyVal:
      def error(problem: Problem): Unit =
        problem.throwableOption match
          case Some(t) =>
            logger.error(problem.toString, t.appendCurrentStackTrace)
          case None =>
            logger.error(problem.toString)

  object syntax:
    extension (logger: ScalaLogger)
      /** Can be used to import these extensions. */
      inline def forceImportExtensions: Unit = ()

      def isEnabled(level: LogLevel): Boolean =
        logger.underlying.isEnabled(level)

      def isEnabled(level: LogLevel, marker: Marker): Boolean =
        logger.underlying.isEnabled(level, marker)

      def log(level: LogLevel, message: => String): Unit =
        logger.underlying.log(level, message)

      def log(level: LogLevel, message: => String, throwable: Throwable): Unit =
        logger.underlying.log(level, message, throwable)

      def log(level: LogLevel, marker: Marker, message: => String): Unit =
        logger.underlying.log(level, marker, message)

      inline def trace(inline msg: String): Unit =
        logger.trace(msg)

      def infoCall[A](body: => A)(implicit src: sourcecode.Name): A =
        infoCall[A](src.value)(body)

      def infoCall[A](functionName: String, args: => Any = "")(body: => A): A =
        logF[SyncIO, A](logger, LogLevel.Info, functionName, args)(SyncIO(body)).unsafeRunSync()

      def infoIO[A](body: IO[A])(using src: sourcecode.Name): IO[A] =
        infoF(functionName = src.value)(body)

      def infoIO[A](functionName: String, args: => Any = "")(body: IO[A]): IO[A] =
        infoF(functionName, args)(body)

      def infoF[F[_], A](body: F[A])(using F: Sync[F], src: sourcecode.Name)
      : F[A] =
        infoF[F, A](functionName = src.value)(body)

      def infoF[F[_], A](functionName: String, args: => Any = "")
        (body: F[A])
        (using F: Sync[F])
      : F[A] =
        logF[F, A](logger, LogLevel.Info, functionName, args)(body)

      def debugCall[A](body: => A)(implicit src: sourcecode.Name): A =
        debugCall[A](src.value)(body)

      def debugCall[A](functionName: String, args: => Any = "")(body: => A): A =
        logF[SyncIO, A](logger, LogLevel.Debug, functionName, args)(SyncIO(body)).unsafeRunSync()

      def debugIO[A](body: IO[A])(using src: sourcecode.Name): IO[A] =
        debugF(body)

      def debugIO[A](functionName: String, args: => Any = "")(body: IO[A]): IO[A] =
        debugF(functionName, args)(body)

      def debugF[F[_], A](body: F[A])(using F: Sync[F], src: sourcecode.Name)
      : F[A] =
        debugF[F, A](functionName = src.value)(body)

      def debugF[F[_], A](functionName: String, args: => Any = "")(body: F[A])(implicit F: Sync[F])
      : F[A] =
        logF[F, A](logger, LogLevel.Debug, functionName, args)(body)

      def debugIOWithResult[A](io: IO[A])(implicit src: sourcecode.Name): IO[A] =
        debugIOWithResult[A](src.value)(io)

      def debugIOWithResult[A](function: String)(body: IO[A])
      : IO[A] =
        logF[IO, A](logger, LogLevel.Debug, function)(body)

      def debugIOWithResult[A](function: String, args: => Any)(io: IO[A])
      : IO[A] =
        logF[IO, A](logger, LogLevel.Debug, function, args)(io)

      def debugIOWithResult[A](function: String, args: => Any = "", result: A => Any = null)
        (io: IO[A])
      : IO[A] =
        logF[IO, A](logger, LogLevel.Debug, function, args, result)(io)

      inline def traceIO[A](inline body: IO[A])(using inline src: sourcecode.Name): IO[A] =
        traceIOif(functionName = src.value)(body)

      inline def traceIO[A](inline functionName: String, inline args: Any = "")(inline body: IO[A])
      : IO[A] =
        traceIOif(functionName, args)(body)

      private inline def traceIOif[A](inline functionName: String, inline args: Any = "")
        (inline body: IO[A])
      : IO[A] =
        if isTraceEnabled /*not deferred*/ then
          traceIO_(functionName, args)(body)
        else
          body

      private def traceIO_[A](functionName: String, args: => Any = "")(body: IO[A]): IO[A] =
        traceF(functionName, args)(body)

      def traceF[F[_], A](body: F[A])(using F: Sync[F], src: sourcecode.Name)
      : F[A] =
        traceF(functionName = src.value)(body)

      def traceF[F[_], A](functionName: String, args: => Any = "")(body: F[A])(implicit F: Sync[F])
      : F[A] =
        logF[F, A](logger, LogLevel.Trace, functionName, args)(body)

      def traceIOWithResult[A](body: IO[A])(implicit src: sourcecode.Name): IO[A] =
        traceIOWithResult[A](src.value, body = body)

      def traceIOWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A](_),
        body: IO[A])
      : IO[A] =
        logF[IO, A](logger, LogLevel.Trace, function, args, result)(body)

      def traceCall[A](body: => A)(implicit src: sourcecode.Name): A =
        traceCall[A](src.value)(body)

      def traceCall[A](functionName: String, args: => Any = "")(body: => A): A =
        logF[SyncIO, A](logger, LogLevel.Trace, functionName, args)(SyncIO(body)).unsafeRunSync()

      def traceCallWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A](_),
        body: => A)
      : A =
        logF[SyncIO, A](logger, LogLevel.Trace, function, args, result)(SyncIO(body))
          .unsafeRunSync()

      /** Log nothing, usable as a equivalent replacement to temporarily suppress logging. */
      inline def noLogF[F[_], A](inline a: F[A]): F[A] = a

      /** Log nothing, usable as a equivalent replacement to temporarily suppress logging. */
      inline def noLogIO[A](@unused inline functionName: String, @unused inline args: => Any = "")
        (inline a: IO[A])
      : IO[A] = a

      def infoResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
        (using F: Sync[F])
      : Resource[F, A] =
        logResource[F, A](logger, LogLevel.Info, function, args)(resource)

      def debugResource[F[_], A](resource: Resource[F, A])
        (using F: Sync[F], tag: Tag[A], src: sourcecode.Name)
      : Resource[F, A] =
        logResource[F, A](logger, LogLevel.Debug)(resource)

      def debugResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
        (using F: Sync[F])
      : Resource[F, A] =
        logResource[F, A](logger, LogLevel.Debug, function, args)(resource)

      def traceResource[F[_], A](resource: Resource[F, A])
        (using F: Sync[F], tag: Tag[A], src: sourcecode.Name)
      : Resource[F, A] =
        logResource[F, A](logger, LogLevel.Trace)(resource)

      def traceResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
        (implicit F: Sync[F])
      : Resource[F, A] =
        logResource[F, A](logger, LogLevel.Trace, function, args)(resource)

      def infoStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
        (using F: Sync[F])
      : Stream[F, A] =
        logStream[F, A](logger, LogLevel.Info, function, args)(stream)

      def debugStream[F[_], A](stream: Stream[F, A])
        (using F: Sync[F], A: Tag[A], src: sourcecode.Name)
      : Stream[F, A] =
        debugStream(s"${src.value}: Stream[_,${A.tag.shortName}]")(stream)

      def debugStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
        (using F: Sync[F])
      : Stream[F, A] =
        logStream[F, A](logger, LogLevel.Debug, function, args)(stream)

      def traceStream[F[_], A](stream: Stream[F, A])
        (using F: Sync[F], src: sourcecode.Name, A: Tag[A])
      : Stream[F, A] =
        traceStream(s"${src.value}: Stream[_,${A.tag.shortName}]")(stream)

      def traceStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
        (using F: Sync[F])
      : Stream[F, A] =
        logStream[F, A](logger, LogLevel.Trace, function, args)(stream)

      def logStart(logLevel: LogLevel, function: String, args: => Any = ""): Unit =
        Logger.logStart(logger, logLevel, function, args)

      def logOutcome[F[_], A](
        logLevel: LogLevel,
        function: String,
        duration: FiniteDuration,
        outcome: Outcome[F, Throwable, A])
      : Unit =
        Logger.logOutcome[F, A](logger, logLevel, function, args = "", duration.pretty + " ", outcome)

      inline def isErrorEnabled: Boolean =
        logger.underlying.isErrorEnabled

      inline def isWarnEnabled: Boolean =
        logger.underlying.isWarnEnabled

      inline def isInfoEnabled: Boolean =
        logger.underlying.isInfoEnabled

      inline def isDebugEnabled: Boolean =
        logger.underlying.isDebugEnabled

      inline def isTraceEnabled: Boolean =
        logger.underlying.isTraceEnabled


    private[log] def logF[F[_], A](
      logger: ScalaLogger,
      logLevel: LogLevel,
      function: String,
      args: => Any = "",
      resultToLoggable: A => Any = null)
      (body: F[A])
      (implicit F: Sync[F])
    : F[A] =
      F.defer:
        if !logger.isEnabled(logLevel) then
          body
        else
          val ctx = new StartReturnLogContext(logger, logLevel, function, args)
          body
            .flatTap:
              case left @ Left(_: Throwable | _: Problem) =>
                F.delay:
                  ctx.logReturn("❓", left)
              case result =>
                F.delay:
                  ctx.logReturn(
                    "",
                    if resultToLoggable eq null then
                      "Completed"
                    else
                      "· " + resultToLoggable(result))
            .guaranteeCase:
              case Outcome.Succeeded(_) => F.unit
              case outcome => F.delay(ctx.logOutcome(outcome))

    private def logResource[F[_], A](logger: ScalaLogger, logLevel: LogLevel)
      (resource: Resource[F, A])
      (using F: Sync[F], tag: Tag[A], src: sourcecode.Name)
    : Resource[F, A] =
        logResource[F, A](logger, logLevel, s"${src.value} :Resource[_,${tag.tag}]"):
          resource

    private def logResource[F[_], A](
      logger: ScalaLogger, logLevel: LogLevel, function: String, args: => Any = "")
      (resource: Resource[F, A])
      (using F: Sync[F])
    : Resource[F, A] =
      for
        a <- Resource.applyFull[F, A]: cancelable =>
          logF(logger, logLevel, function + ".acquire", args):
            cancelable:
              resource.allocatedCase
          .map { case (a, release) =>
            a ->
              (exitCase =>
                logF[F, Unit](logger, logLevel, function + ".release", args):
                  release(exitCase))
          }
        //_ <- loggingResource[F](logger, logLevel, function + ".use", args)
      yield
        a

    private def loggingResource[F[_]](
      logger: ScalaLogger, logLevel: LogLevel, function: String, args: => Any = "")
      (using F: Sync[F])
    : Resource[F, Unit] =
      Resource
        .makeCase(
          acquire = F.delay:
            logger.isEnabled(logLevel) ?
              new StartReturnLogContext(logger, logLevel, function, args))(
          release = (maybeCtx, exitCase) => F.delay:
            for ctx <- maybeCtx do ctx.logOutcome(exitCase.toOutcome))
        .map(_ => ())

    private def logStream[F[_], A](logger: ScalaLogger, logLevel: LogLevel, function: String,
      args: => Any = "")
      (stream: Stream[F, A])
      (using F: Sync[F])
    : Stream[F, A] =
      Stream.suspend:
        if !logger.isEnabled(logLevel) then
          stream
        else
          var n, chunks = 0L
          val startedAt = Deadline.now
          stream
            .onStart(F.delay:
              Logger.logStart(logger, logLevel, function, args))
            .mapChunks: chunk =>
              n += chunk.size
              chunks += 1
              chunk
            .onFinalizeCase(exitCase => F.delay:
              Logger.logOutcome(logger, logLevel, function, args, duration = "",
                exitCase.toOutcome[F],
                result =
                  s"$chunks chunks, ${itemsPerSecondString(startedAt.elapsed, n, "elems")}"))

  private final class StartReturnLogContext(logger: ScalaLogger, logLevel: LogLevel,
    function: String, args: => Any = ""):

    logStart(logger, logLevel, function, args)
    private val startedAt = System.nanoTime()

    private def duration: String =
      if startedAt == 0 then
        ""
      else
        (System.nanoTime() - startedAt).ns.pretty + " "

    def logOutcome[F[_], A](outcome: Outcome[F, Throwable, A]): Unit =
      Logger.logOutcome(logger, logLevel, function, "", duration, outcome)

    def logReturn(marker: String, msg: AnyRef): Unit =
      Logger.logReturn(logger, logLevel, function, "", duration, marker, msg)

  private def logStart(logger: ScalaLogger, logLevel: LogLevel, function: String, args: => Any = "")
  : Unit =
    lazy val argsString = args match
      case null => "null"
      case o =>
        try o.toString
        catch case t: Throwable => t.toStringWithCauses

    if argsString.isEmpty then
      logLevel match
        case LogLevel.LogNone =>
        case LogLevel.Trace => logger.trace(s"↘ $function ↘")
        case LogLevel.Debug => logger.debug(s"↘ $function ↘")
        case LogLevel.Info  => logger.info (s"↘ $function ↘")
        case LogLevel.Warn  => logger.warn (s"↘ $function ↘")
        case LogLevel.Error => logger.error(s"↘ $function ↘")
    else
      logLevel match
        case LogLevel.LogNone =>
        case LogLevel.Trace => logger.trace(s"↘ $function($argsString) ↘")
        case LogLevel.Debug => logger.debug(s"↘ $function($argsString) ↘")
        case LogLevel.Info  => logger.info (s"↘ $function($argsString) ↘")
        case LogLevel.Warn  => logger.warn (s"↘ $function($argsString) ↘")
        case LogLevel.Error => logger.error(s"↘ $function($argsString) ↘")

  private def logOutcome[F[_], A](
    logger: ScalaLogger,
    logLevel: LogLevel,
    function: String,
    args: => Any,
    duration: String,
    outcome: Outcome[F, Throwable, A],
    result: => String = "")
  : Unit =
    lazy val result_ = result
    def res = result_.nonEmpty ?? " • " + result_
    outcome match
      case Outcome.Errored(t) =>
        logReturn(logger, logLevel, function, args, duration, "💥️", t.toStringWithCauses + res)
      case Outcome.Canceled() =>
        logReturn(logger, logLevel, function, args, duration, "⚫️", "Canceled" + res)
      case Outcome.Succeeded(_) =>
        logReturn(logger, logLevel, function, args, duration, "", "Completed" + res)

  private def logReturn(
    logger: ScalaLogger,
    logLevel: LogLevel,
    function: String,
    args: => Any = "",
    duration: String,
    marker: String,
    msg: AnyRef)
  : Unit =
    lazy val argsString = args.toString
    if argsString.isEmpty then
      logLevel match
        case LogLevel.LogNone =>
        case LogLevel.Trace => logger.trace(s"↙$marker $function => $duration$msg ↙")
        case LogLevel.Debug => logger.debug(s"↙$marker $function => $duration$msg ↙")
        case LogLevel.Info  => logger.info (s"↙$marker $function => $duration$msg ↙")
        case LogLevel.Warn  => logger.warn (s"↙$marker $function => $duration$msg ↙")
        case LogLevel.Error => logger.error(s"↙$marker $function => $duration$msg ↙")
    else
      logLevel match
        case LogLevel.LogNone =>
        case LogLevel.Trace => logger.trace(s"↙$marker $function($argsString) => $duration$msg ↙")
        case LogLevel.Debug => logger.debug(s"↙$marker $function($argsString) => $duration$msg ↙")
        case LogLevel.Info  => logger.info (s"↙$marker $function($argsString) => $duration$msg ↙")
        case LogLevel.Warn  => logger.warn (s"↙$marker $function($argsString) => $duration$msg ↙")
        case LogLevel.Error => logger.error(s"↙$marker $function($argsString) => $duration$msg ↙")
