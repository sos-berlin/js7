package js7.base.log

import cats.effect.implicits.monadCancelOps
import cats.effect.{IO, Outcome, Resource, Sync, SyncIO}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import com.typesafe.scalalogging.Logger as ScalaLogger
import fs2.Stream
import izumi.reflect.Tag
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.log.Slf4jUtils.syntax.*
import js7.base.problem.Problem
import js7.base.system.startup.StartUp
import js7.base.time.ScalaTime.{DurationRichLong, RichDeadline, RichDuration}
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichBoolean, RichJavaClass, RichThrowable}
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.{Atomic, Once, Tests, Worry}
import org.slf4j.{LoggerFactory, Marker, MarkerFactory}
import scala.annotation.unused
import scala.concurrent.duration.Deadline
import scala.reflect.ClassTag

type Logger = ScalaLogger

object Logger extends AdHocLogger:

  private val ifNotInitialized = new Once
  private val initLogged = Atomic(false)
  private val ifNotMissingInitializingWarned = new Once
  private val ioSync: Sync[IO] = summon[Sync[IO]]

  Slf4jUtils.initialize()

  lazy val empty: ScalaLogger =
    warnIfNotInitialized(classOf[this.type])
    ScalaLogger(org.slf4j.helpers.NOPLogger.NOP_LOGGER)

  def resource[F[_]](name: String)(using F: Sync[F]): Resource[F, Unit] =
    Resource(F.delay:
      initialize(name) -> F.delay:
        shutdown())

  def initialize(name: String, suppressInfo: Boolean = false): Unit =
    ifNotInitialized:
      Log4j.initialize(name)
    if !initLogged.getAndSet(true) then
      if !suppressInfo then
        StartUp.logStartUpLine(name)
      Tests.log()

  def shutdown(fast: Boolean = false, suppressLogging: Boolean = false): Unit =
    Log4j.shutdown(fast = fast, suppressLogging = suppressLogging)

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
  val Heartbeat: Marker = MarkerFactory.getMarker("Heartbeat")
  val Stream: Marker = MarkerFactory.getMarker("Stream")

  def apply[A: ClassTag]: ScalaLogger =
    apply(implicitClass[A])

  def apply(c: Class[?]): ScalaLogger =
    apply(normalizeClassName(c))

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

  private def warnIfNotInitialized(cls: Class[?] | Null, prefix: String = ""): Unit =
    if !ifNotInitialized.isInitialized then
      ifNotMissingInitializingWarned:
        val classArg = if cls == null then "" else s"[${cls.scalaName}]"
        val prefixArg = prefix.nonEmpty ?? s"(\"$prefix\")"
        val msg =
          s"Log4j will not be properly initialized because Logger$classArg$prefixArg is being created too early"
        System.err.println(msg)
        ScalaLogger[this.type].error(msg, Exception(msg))

  /** Removes '$' from Scala's companion object class. */
  private def normalizeClassName(c: Class[?]): String =
    c.getName.stripSuffix("$").replace('$', '.')

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

      inline def isEnabled(level: LogLevel): Boolean =
        logger.underlying.isEnabled(level)

      inline def isEnabled(level: LogLevel, marker: Marker): Boolean =
        logger.underlying.isEnabled(level, marker)

      inline def log(level: LogLevel, message: => String): Unit =
        logger.underlying.log(level, message)

      inline def log(level: LogLevel, message: => String, throwable: Throwable): Unit =
        logger.underlying.log(level, message, throwable)

      inline def log(level: LogLevel, marker: Marker, message: => String): Unit =
        logger.underlying.log(level, marker, message)

      def infoCall[A](body: => A)(implicit src: sourcecode.Name): A =
        infoCall[A](src.value)(body)

      def infoCall[A](functionName: String, args: => Any = "")(body: => A): A =
        logF[SyncIO, A](logger, LogLevel.Info, functionName, args)(SyncIO(body)).unsafeRunSync()

      inline def infoCallWithResult[A](inline body: => A)(implicit inline src: sourcecode.Name)
      : A =
        infoCallWithResult(src.value)(body)

      inline def infoCallWithResult[A](inline function: String)(inline body: => A): A =
        infoCallWithResult[A](function, body = body)

      def infoCallWithResult[A](function: String, args: => Any)(body: => A): A =
        infoCallWithResult[A](function, args, body = body)

      def infoCallWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A],
        marker: Marker | Null = null,
        body: => A)
      : A =
        logF[SyncIO, A](logger, LogLevel.Info, function, args, result, marker)(SyncIO(body))
          .unsafeRunSync()

      def infoIO[A](body: IO[A])(using src: sourcecode.Name): IO[A] =
        infoF(functionName = src.value)(body)

      def infoIO[A](functionName: String, args: => Any = "")(body: IO[A]): IO[A] =
        infoF(functionName, args)(body)

      def infoIOWithResult[A](body: IO[A])(implicit src: sourcecode.Name): IO[A] =
        infoIOWithResult[A](src.value, body = body)

      def infoIOWithResult[A](function: String)(body: IO[A]): IO[A] =
        infoIOWithResult[A](function, body = body)

      def infoIOWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A],
        marker: Marker | Null = null,
        body: IO[A])
      : IO[A] =
        logF[IO, A](logger, LogLevel.Info, function, args, result, marker = marker)(body)

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
        logF[IO, A](logger, LogLevel.Debug, function, resultToLoggable = identity)(body)

      def debugIOWithResult[A](function: String, args: => Any)(io: IO[A])
      : IO[A] =
        logF[IO, A](logger, LogLevel.Debug, function, args, resultToLoggable = identity)(io)

      def debugIOWithResult[A](
        function: String,
        args: => Any = "",
        result: (A => Any) | Null  = null)
        (io: IO[A])
      : IO[A] =
        logF[IO, A](logger, LogLevel.Debug, function, args, result)(io)

      def traceIO[A](body: IO[A])(using src: sourcecode.Name): IO[A] =
        traceF(functionName = src.value)(body)

      def traceIO[A](functionName: String, args: Any = "")(body: IO[A]): IO[A] =
        traceF(functionName, args)(body)

      def traceF[F[_], A](body: F[A])(using F: Sync[F], src: sourcecode.Name): F[A] =
        traceF(functionName = src.value)(body)

      def traceF[F[_], A](functionName: String, args: => Any = "")(body: F[A])(implicit F: Sync[F])
      : F[A] =
        logF[F, A](logger, LogLevel.Trace, functionName, args)(body)

      def traceIOWithResult[A](body: IO[A])(implicit src: sourcecode.Name): IO[A] =
        traceIOWithResult[A](src.value, body = body)

      def traceIOWithResult[A](function: String)(body: IO[A]): IO[A] =
        traceIOWithResult[A](function, body = body)

      def traceIOWithResult[A](function: String, args: => Any)(body: IO[A]): IO[A] =
        traceIOWithResult[A](function, args = args, body = body)

      def traceIOWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A],
        marker: Marker | Null = null,
        body: IO[A])
      : IO[A] =
        logF[IO, A](logger, LogLevel.Trace, function, args, result, marker = marker)(body)

      inline def traceCall[A](body: => A)(implicit src: sourcecode.Name): A =
        traceCall[A](src.value)(body)

      def traceCall[A](functionName: String, args: => Any = "")(body: => A): A =
        logF[SyncIO, A](logger, LogLevel.Trace, functionName, args)(SyncIO(body)).unsafeRunSync()

      inline def traceCallWithResult[A](inline body: => A)(implicit inline src: sourcecode.Name)
      : A =
        traceCallWithResult(src.value)(body)

      def traceCallWithResult[A](function: String)(body: => A): A =
        traceCallWithResult[A](function, body = body)

      def traceCallWithResult[A](function: String, args: => Any)(body: => A): A =
        traceCallWithResult[A](function, args, body = body)

      def traceCallWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A],
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

      def infoResource[F[_], A](resource: Resource[F, A])
        (using F: Sync[F], tag: Tag[A], src: sourcecode.Name)
      : Resource[F, A] =
        logResource[F, A](logger, LogLevel.Info)(resource)

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
        debugStream(s"${src.value}: Stream[${A.tag.shortName}]")(stream)

      def debugStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
        (using F: Sync[F])
      : Stream[F, A] =
        logStream[F, A](logger, LogLevel.Debug, function, args)(stream)

      def traceStream[F[_], A](stream: Stream[F, A])
        (using F: Sync[F], src: sourcecode.Name, A: Tag[A])
      : Stream[F, A] =
        traceStream(s"${src.value}: Stream[${A.tag.shortName}]")(stream)

      def traceStream[F[_], A](function: String, args: => Any = "")(stream: Stream[F, A])
        (using F: Sync[F])
      : Stream[F, A] =
        logStream[F, A](logger, LogLevel.Trace, function, args)(stream)

      def logStart(logLevel: LogLevel, function: String, args: => Any = ""): Unit =
        Logger.logStart(logger, logLevel, marker = null, function, args)

      //def logOutcome[F[_], A](
      //  logLevel: LogLevel,
      //  marker: Marker | Null,
      //  function: String,
      //  duration: FiniteDuration,
      //  outcome: Outcome[F, Throwable, A])
      //: Unit =
      //  Logger.logOutcome[F, A](logger, logLevel, marker, function, args = "", duration.pretty + " ", outcome)

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
      resultToLoggable: (A => Any) | Null = null,
      marker: Marker | Null = null)
      (body: F[A])
      (implicit F: Sync[F])
    : F[A] =
      F.defer:
        if !logger.isEnabled(logLevel) then
          body
        else
          val ctx = new StartReturnLogContext(logger, logLevel, marker, function, args)
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
      logResource[F, A](logger, logLevel, s"${src.value} :Resource[${tag.tag}]"):
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
          .map: (a, release) =>
            a ->
              (exitCase =>
                logF[F, Unit](logger, logLevel, function + ".release", args):
                  release(exitCase)
                .pipeIf(F eq ioSync):
                  _.asInstanceOf[IO[Unit]]
                    .logWhenItTakesLonger(s"release $function",
                      Worry.Default.copy(maxLogLevel = logLevel))
                    .asInstanceOf[F[Unit]])
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
              new StartReturnLogContext(logger, logLevel, marker = null, function, args))(
          release = (maybeCtx, exitCase) => F.delay:
            for ctx <- maybeCtx do ctx.logOutcome(exitCase.toOutcome))
        .map(_ => ())

    private def logStream[F[_], A](logger: ScalaLogger, logLevel: LogLevel, function: String,
      args: => Any = "")
      (stream: Stream[F, A])
      (using F: Sync[F])
    : Stream[F, A] =
      fs2.Stream.suspend:
        if !logger.isEnabled(logLevel) then
          stream
        else
          var n, chunks = 0L
          val startedAt = Deadline.now
          stream
            .onStart(F.delay:
              Logger.logStart(logger, logLevel, marker = null, function, args))
            .mapChunks: chunk =>
              n += chunk.size
              chunks += 1
              chunk
            .onFinalizeCase(exitCase => F.delay:
              Logger.logOutcome(logger, logLevel, marker = null, function, args, duration = "",
                exitCase.toOutcome[F],
                result =
                  s"$chunks chunks, ${itemsPerSecondString(startedAt.elapsed, n, "elems")}"))

  private final class StartReturnLogContext(
    logger: ScalaLogger, logLevel: LogLevel, marker: Marker | Null,
    function: String, args: => Any = ""):

    logStart(logger, logLevel, marker, function, args)
    private val startedAt = System.nanoTime()

    private def duration: String =
      if startedAt == 0 then
        ""
      else
        (System.nanoTime() - startedAt).ns.pretty + " "

    inline def logOutcome[F[_], A](outcome: Outcome[F, Throwable, A]): Unit =
      Logger.logOutcome(logger, logLevel, marker, function, "", duration, outcome)

    inline def logReturn(symbol: String, inline result: AnyRef): Unit =
      Logger.logReturn(logger, logLevel, marker, function, "", duration, symbol, result)

  private def logStart(logger: ScalaLogger, logLevel: LogLevel, marker: Marker | Null,
    function: String, args: => Any = "")
  : Unit =
    lazy val argsString = args match
      case null => "null"
      case o =>
        try o.toString
        catch case t: Throwable => t.toStringWithCauses

    marker match
      case null =>
        if argsString.isEmpty then
          logLevel match
            case LogLevel.None =>
            case LogLevel.Trace => logger.trace(s"↘ $function ↘")
            case LogLevel.Debug => logger.debug(s"↘ $function ↘")
            case LogLevel.Info  => logger.info (s"↘ $function ↘")
            case LogLevel.Warn  => logger.warn (s"↘ $function ↘")
            case LogLevel.Error => logger.error(s"↘ $function ↘")
        else
          logLevel match
            case LogLevel.None =>
            case LogLevel.Trace => logger.trace(s"↘ $function($argsString) ↘")
            case LogLevel.Debug => logger.debug(s"↘ $function($argsString) ↘")
            case LogLevel.Info  => logger.info (s"↘ $function($argsString) ↘")
            case LogLevel.Warn  => logger.warn (s"↘ $function($argsString) ↘")
            case LogLevel.Error => logger.error(s"↘ $function($argsString) ↘")

      case marker: Marker =>
        if argsString.isEmpty then
          logLevel match
            case LogLevel.None =>
            case LogLevel.Trace => logger.trace(marker, s"↘ $function ↘")
            case LogLevel.Debug => logger.debug(marker, s"↘ $function ↘")
            case LogLevel.Info  => logger.info (marker, s"↘ $function ↘")
            case LogLevel.Warn  => logger.warn (marker, s"↘ $function ↘")
            case LogLevel.Error => logger.error(marker, s"↘ $function ↘")
        else
          logLevel match
            case LogLevel.None =>
            case LogLevel.Trace => logger.trace(marker, s"↘ $function($argsString) ↘")
            case LogLevel.Debug => logger.debug(marker, s"↘ $function($argsString) ↘")
            case LogLevel.Info  => logger.info (marker, s"↘ $function($argsString) ↘")
            case LogLevel.Warn  => logger.warn (marker, s"↘ $function($argsString) ↘")
            case LogLevel.Error => logger.error(marker, s"↘ $function($argsString) ↘")

  private def logOutcome[F[_], A](
    logger: ScalaLogger,
    logLevel: LogLevel,
    marker: Marker | Null,
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
        logReturn(logger, logLevel, marker, function, args, duration, "💥️", t.toStringWithCauses + res, t)
      case Outcome.Canceled() =>
        logReturn(logger, logLevel, marker, function, args, duration, "◼️ ", "Canceled" + res)
      case Outcome.Succeeded(_) =>
        logReturn(logger, logLevel, marker, function, args, duration, "", "Completed" + res)

  private def logReturn(
    logger: ScalaLogger,
    logLevel: LogLevel,
    marker: Marker | Null,
    function: String,
    args: => Any = "",
    duration: => String,
    symbol: String,
    result: => Any,
    t: Throwable | Null = null)
  : Unit =
    lazy val argsString = args.toString
    marker match
      case null =>
        if argsString.isEmpty then
          logLevel match
            case LogLevel.None =>
            case LogLevel.Trace => logger.trace(s"↙$symbol $function => $duration$result ↙", t)
            case LogLevel.Debug => logger.debug(s"↙$symbol $function => $duration$result ↙", t)
            case LogLevel.Info  => logger.info (s"↙$symbol $function => $duration$result ↙", t)
            case LogLevel.Warn  => logger.warn (s"↙$symbol $function => $duration$result ↙", t)
            case LogLevel.Error => logger.error(s"↙$symbol $function => $duration$result ↙", t)
        else
          logLevel match
            case LogLevel.None =>
            case LogLevel.Trace => logger.trace(s"↙$symbol $function($argsString) => $duration$result ↙", t)
            case LogLevel.Debug => logger.debug(s"↙$symbol $function($argsString) => $duration$result ↙", t)
            case LogLevel.Info  => logger.info (s"↙$symbol $function($argsString) => $duration$result ↙", t)
            case LogLevel.Warn  => logger.warn (s"↙$symbol $function($argsString) => $duration$result ↙", t)
            case LogLevel.Error => logger.error(s"↙$symbol $function($argsString) => $duration$result ↙", t)

      case marker: Marker =>
        if argsString.isEmpty then
          logLevel match
            case LogLevel.None =>
            case LogLevel.Trace => logger.trace(marker, s"↙$symbol $function => $duration$result ↙", t)
            case LogLevel.Debug => logger.debug(marker, s"↙$symbol $function => $duration$result ↙", t)
            case LogLevel.Info  => logger.info (marker, s"↙$symbol $function => $duration$result ↙", t)
            case LogLevel.Warn  => logger.warn (marker, s"↙$symbol $function => $duration$result ↙", t)
            case LogLevel.Error => logger.error(marker, s"↙$symbol $function => $duration$result ↙", t)
        else
          logLevel match
            case LogLevel.None =>
            case LogLevel.Trace => logger.trace(marker, s"↙$symbol $function($argsString) => $duration$result ↙", t)
            case LogLevel.Debug => logger.debug(marker, s"↙$symbol $function($argsString) => $duration$result ↙", t)
            case LogLevel.Info  => logger.info (marker, s"↙$symbol $function($argsString) => $duration$result ↙", t)
            case LogLevel.Warn  => logger.warn (marker, s"↙$symbol $function($argsString) => $duration$result ↙", t)
            case LogLevel.Error => logger.error(marker, s"↙$symbol $function($argsString) => $duration$result ↙", t)
