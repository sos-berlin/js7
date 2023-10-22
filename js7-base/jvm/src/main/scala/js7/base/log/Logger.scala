package js7.base.log

import cats.{Applicative, MonadError}
import cats.effect.{IO, MonadCancel, Outcome, OutcomeIO, Resource, Sync, SyncIO}
import cats.syntax.flatMap.*
import com.typesafe.scalalogging.Logger as ScalaLogger
import js7.base.log.Slf4jUtils.syntax.*
import js7.base.problem.Problem
import js7.base.system.startup.StartUp
import js7.base.time.ScalaTime.{DurationRichLong, RichDuration}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.{Once, Tests}
import fs2.Stream
import org.slf4j.{LoggerFactory, Marker, MarkerFactory}
import scala.reflect.ClassTag

object Logger:

  private val ifNotInitialized = new Once

  Slf4jUtils.initialize()

  lazy val empty: ScalaLogger =
    ScalaLogger(org.slf4j.helpers.NOPLogger.NOP_LOGGER)

  def initialize(name: String): Unit =
    ifNotInitialized:
      Log4j.initialize(name)
      Logger[this.type].info(StartUp.startUpLine(name))
      Tests.log()

  //val Timing: Marker = MarkerFactory.getMarker("Timing")
  //val Event: Marker = MarkerFactory.getMarker("Event")
  val Actor: Marker = MarkerFactory.getMarker("Actor")
  val Java: Marker = MarkerFactory.getMarker("Java")
  //val Repo: Marker = MarkerFactory.getMarker("Repo")
  val SignatureVerified: Marker = MarkerFactory.getMarker("SignatureVerified")

  def apply[A: ClassTag]: ScalaLogger =
    apply(implicitClass[A])

  def apply(c: Class[?]): ScalaLogger =
    ScalaLogger(normalizeClassName(c))

  def apply(name: String): ScalaLogger =
    ScalaLogger(name)

  def withPrefix[A: ClassTag](prefix: String): ScalaLogger =
    withPrefix(implicitClass[A], prefix)

  def withPrefix(c: Class[?], prefix: String): ScalaLogger =
    if prefix.isEmpty then
      apply(c)
    else
      ScalaLogger(new ConvertingLogger.Prefixed(
        prefix,
        LoggerFactory.getLogger(normalizeClassName(c))))

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
    implicit final class RichScalaLogger(private val logger: ScalaLogger) extends AnyVal:
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

      def infoIO[A](body: IO[A])(src: sourcecode.Name): IO[A] =
        infoF(functionName = src.value)(body)

      def infoIO[A](functionName: String, args: => Any = "")(body: IO[A]): IO[A] =
        infoF(functionName, args)(body)

      def infoF[F[_], A](body: F[A])(using F: Sync[F], src: sourcecode.Name)
      : F[A] =
        infoF[F, A](functionName = src.value)(body)

      def infoF[F[_], A](functionName: String, args: => Any = "")(body: F[A])(implicit F: Sync[F])
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

      def traceIO[A](body: IO[A])(using src: sourcecode.Name): IO[A] =
        traceF(functionName = src.value)(body)

      def traceIO[A](functionName: String, args: => Any = "")(body: IO[A]): IO[A] =
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

      def infoResource[A](function: String, args: => Any = "")(resource: Resource[IO, A])
      : Resource[IO, A] =
        logResourceUse[IO, A](logger, LogLevel.Info, function, args)(resource)

      def debugResource[A](resource: Resource[IO, A])(implicit src: sourcecode.Name)
      : Resource[IO, A] =
        debugResource(src.value + ".use")(resource)

      def debugResource[A](function: String, args: => Any = "")(resource: Resource[IO, A])
      : Resource[IO, A] =
        logResourceUse[IO, A](logger, LogLevel.Debug, function, args)(resource)

      def traceResource[F[_], A](resource: Resource[F, A])
        (implicit F: Applicative[F] & Sync[F], src: sourcecode.Name)
      : Resource[F, A] =
        traceResource(src.value + ".use")(resource)

      def traceResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
        (implicit F: Applicative[F] & Sync[F])
      : Resource[F, A] =
        logResourceUse[F, A](logger, LogLevel.Trace, function, args)(resource)

      def infoStream[A](function: String, args: => Any = "")(stream: Stream[IO, A])
      : Stream[IO, A] =
        logStream[A](logger, LogLevel.Info, function, args)(stream)

      def debugStream[A](stream: Stream[IO, A])(implicit src: sourcecode.Name)
      : Stream[IO, A] =
        debugStream(src.value + ": Stream")(stream)

      def debugStream[A](function: String, args: => Any = "")(stream: Stream[IO, A])
      : Stream[IO, A] =
        logStream[A](logger, LogLevel.Debug, function, args)(stream)

      def traceStream[A](stream: Stream[IO, A])(implicit src: sourcecode.Name)
      : Stream[IO, A] =
        traceStream(src.value + ": Stream")(stream)

      def traceStream[A](function: String, args: => Any = "")(stream: Stream[IO, A])
      : Stream[IO, A] =
        logStream[A](logger, LogLevel.Trace, function, args)(stream)

    private def logF[F[_] <: MonadError[F, Throwable], A](
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
                F.delay(ctx.logReturn("❓", left))
              case result =>
                F.delay(ctx.logReturn(
                  "",
                  if resultToLoggable eq null then
                    "Completed"
                  else
                    resultToLoggable(result).toString))
            .guaranteeCase:
              case Outcome.Succeeded(_) => F.unit
              case outcome => F.delay(ctx.logOutcome(outcome))

    private def logResourceUse[F[_], A](logger: ScalaLogger, logLevel: LogLevel, function: String,
      args: => Any = "")
      (resource: Resource[F, A])
      (implicit F: Applicative[F] & Sync[F])
    : Resource[F, A] =
      Resource
        .makeCase(
          acquire = F.delay(
            if !logger.isEnabled(logLevel) then
              None
            else
              Some(new StartReturnLogContext(logger, logLevel, function, args))))(
          release = (maybeCtx, exitCase) =>
            F.delay(
              for ctx <- maybeCtx do ctx.logOutcome(exitCase.toOutcome)))
        .flatMap(_ => resource)

    private def logStream[A](logger: ScalaLogger, logLevel: LogLevel, function: String,
      args: => Any = "")
      (stream: Stream[IO, A])
    : Stream[IO, A] =
      stream
        .doOnSubscribe(
          IO.whenA(logger.isEnabled(logLevel))(IO(
            logStart(logger, logLevel, function, args))))
        .guaranteeCase(exitCase =>
          IO.whenA(logger.isEnabled(logLevel))(IO(
            logOutcome(logger, logLevel, function, args, duration = "", exitCase))))

  private final class StartReturnLogContext(logger: ScalaLogger, logLevel: LogLevel,
    function: String, args: => Any = ""):
    logStart(logger, logLevel, function, args)
    private val startedAt = System.nanoTime()

    private def duration: String =
      if startedAt == 0 then
        ""
      else
        (System.nanoTime() - startedAt).ns.pretty + " "

    def logOutcome[A](outcome: OutcomeIO[A]): Unit =
      Logger.logOutcome(logger, logLevel, function, "", duration, outcome)

    def logReturn(marker: String, msg: AnyRef): Unit =
      Logger.logReturn(logger, logLevel, function, "", duration, marker, msg)

  private def logStart(logger: ScalaLogger, logLevel: LogLevel, function: String,
    args: => Any = "")
  : Unit =
    lazy val argsString = args.toString
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

  private def logOutcome[A](
    logger: ScalaLogger,
    logLevel: LogLevel,
    function: String,
    args: => Any,
    duration: String,
    exitCase: OutcomeIO[A])
  : Unit =
    exitCase match
      case Outcome.Errored(t) =>
        logReturn(logger, logLevel, function, args, duration, "💥️", t.toStringWithCauses)
      case Outcome.Canceled() =>
        logReturn(logger, logLevel, function, args, duration, "⚫️", "Canceled")
      case Outcome.Succeeded(_) =>
        logReturn(logger, logLevel, function, args, duration, "", "Completed")

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
