package js7.base.log

import cats.Applicative
import cats.effect.ExitCase.{Canceled, Completed, Error}
import cats.effect.syntax.bracket.*
import cats.effect.{ExitCase, Resource, Sync, SyncIO}
import cats.syntax.flatMap.*
import com.typesafe.scalalogging.Logger as ScalaLogger
import js7.base.log.LogLevel.syntax.*
import js7.base.problem.Problem
import js7.base.time.ScalaTime.{DurationRichLong, RichDuration}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.StackTraces.StackTraceThrowable
import monix.eval.Task
import monix.reactive.Observable
import org.slf4j.{LoggerFactory, Marker, MarkerFactory}
import scala.reflect.ClassTag

object Logger
{
  Slf4jUtils.initialize()

  lazy val empty: ScalaLogger =
    ScalaLogger(org.slf4j.helpers.NOPLogger.NOP_LOGGER)

  def initialize(): Unit =
    Log4j.initialize()

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
    if (prefix.isEmpty)
      apply(c)
    else
      ScalaLogger(new ConvertingLogger.Prefixed(
        prefix,
        LoggerFactory.getLogger(normalizeClassName(c))))

  /** Removes '$' from Scala's companion object class. */
  private def normalizeClassName(c: Class[?]): String =
    c.getName stripSuffix "$"

  object ops {
    implicit final class RichScalaLogger(private val logger: ScalaLogger) extends AnyVal
    {
      def error(problem: Problem): Unit =
        problem.throwableOption match {
          case Some(t) =>
            logger.error(problem.toString, t.appendCurrentStackTrace)
          case None =>
            logger.error(problem.toString)
        }
    }
  }

  object syntax {
    implicit final class RichScalaLogger(private val logger: ScalaLogger) extends AnyVal
    {
      def infoTask[A](functionName: String, args: => Any = "")(task: Task[A]): Task[A] =
        logF[Task, A](logger, LogLevel.Info, functionName, args)(task)

      def debugTask[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        debugTask(src.value)(task)

      def debugTask[A](functionName: String, args: => Any = "")(task: Task[A]): Task[A] =
        logF[Task, A](logger, LogLevel.Debug, functionName, args)(task)

      def debugCall[A](body: => A)(implicit src: sourcecode.Name): A =
        debugCall[A](src.value)(body)

      def debugCall[A](functionName: String, args: => Any = "")(body: => A): A =
        logF[SyncIO, A](logger, LogLevel.Debug, functionName, args)(SyncIO(body)).unsafeRunSync()

      def debugF[F[_], A](functionName: String, args: => Any = "")(body: F[A])(implicit F: Sync[F])
      : F[A] =
        logF[F, A](logger, LogLevel.Debug, functionName, args)(body)

      def debugTaskWithResult[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        debugTaskWithResult[A](src.value)(task)

      def debugTaskWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A](_))
        (task: Task[A])
      : Task[A] =
        logF[Task, A](logger, LogLevel.Debug, function, args, result)(task)

      def traceTask[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        traceTask(src.value)(task)

      def traceTask[A](function: String, args: => Any = "")(task: Task[A]): Task[A] =
        logF[Task, A](logger, LogLevel.Trace, function, args)(task)

      def traceF[F[_], A](functionName: String, args: => Any = "")(body: F[A])(implicit F: Sync[F])
      : F[A] =
        logF[F, A](logger, LogLevel.Trace, functionName, args)(body)

      def traceTaskWithResult[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        traceTaskWithResult[A](src.value, task = task)

      def traceTaskWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A](_),
        task: Task[A])
      : Task[A] =
        logF[Task, A](logger, LogLevel.Trace, function, args, result)(task)

      def traceCall[A](body: => A)(implicit src: sourcecode.Name): A =
        traceCall[A](src.value)(body)

      def traceCall[A](functionName: String, args: => Any = "")(body: => A): A =
        logF[SyncIO, A](logger, LogLevel.Trace, functionName, args)(SyncIO(body)).unsafeRunSync()

      def infoResource[A](function: String, args: => Any = "")(resource: Resource[Task, A])
      : Resource[Task, A] =
        logResourceUse[Task, A](logger, LogLevel.Info, function, args)(resource)

      def debugResource[A](resource: Resource[Task, A])(implicit src: sourcecode.Name)
      : Resource[Task, A] =
        debugResource(src.value + ".use")(resource)

      def debugResource[A](function: String, args: => Any = "")(resource: Resource[Task, A])
      : Resource[Task, A] =
        logResourceUse[Task, A](logger, LogLevel.Debug, function, args)(resource)

      def traceResource[F[_], A](resource: Resource[F, A])
        (implicit F: Applicative[F] & Sync[F], src: sourcecode.Name)
      : Resource[F, A] =
        traceResource(src.value + ".use")(resource)

      def traceResource[F[_], A](function: String, args: => Any = "")(resource: Resource[F, A])
        (implicit F: Applicative[F] & Sync[F])
      : Resource[F, A] =
        logResourceUse[F, A](logger, LogLevel.Trace, function, args)(resource)

      def infoObservable[A](function: String, args: => Any = "")(observable: Observable[A])
      : Observable[A] =
        logObservable[A](logger, LogLevel.Info, function, args)(observable)

      def debugObservable[A](observable: Observable[A])(implicit src: sourcecode.Name)
      : Observable[A] =
        debugObservable(src.value + ": Observable")(observable)

      def debugObservable[A](function: String, args: => Any = "")(observable: Observable[A])
      : Observable[A] =
        logObservable[A](logger, LogLevel.Debug, function, args)(observable)

      def traceObservable[A](observable: Observable[A])(implicit src: sourcecode.Name)
      : Observable[A] =
        traceObservable(src.value + ": Observable")(observable)

      def traceObservable[A](function: String, args: => Any = "")(observable: Observable[A])
      : Observable[A] =
        logObservable[A](logger, LogLevel.Trace, function, args)(observable)
    }

    private def logF[F[_], A](
      logger: ScalaLogger,
      logLevel: LogLevel,
      function: String,
      args: => Any = "",
      resultToLoggable: A => Any = null)
      (body: F[A])
      (implicit F: Sync[F])
    : F[A] =
      F.defer {
        if (!logger.isEnabled(logLevel))
          body
        else {
          val ctx = new StartReturnLogContext(logger, logLevel, function, args)
          body
            .flatTap {
              case left @ Left(_: Throwable | _: Problem) =>
                F.delay(ctx.logReturn("❓", left))
              case result =>
                F.delay(ctx.logReturn(
                  "",
                  if (resultToLoggable eq null)
                    "Completed"
                  else
                    resultToLoggable(result).toString))
            }
            .guaranteeCase {
              case Completed => F.unit
              case exitCase => F.delay(ctx.logExitCase(exitCase))
            }
        }
      }

    private def logResourceUse[F[_], A](logger: ScalaLogger, logLevel: LogLevel, function: String,
      args: => Any = "")
      (resource: Resource[F, A])
      (implicit F: Applicative[F] & Sync[F])
    : Resource[F, A] =
      Resource
        .makeCase(
          acquire = F.delay(
            if (!logger.isEnabled(logLevel))
              None
            else
              Some(new StartReturnLogContext(logger, logLevel, function, args))))(
          release = (maybeCtx, exitCase) =>
            F.delay(
              for (ctx <- maybeCtx) ctx.logExitCase(exitCase)))
        .flatMap(_ => resource)

    private def logObservable[A](logger: ScalaLogger, logLevel: LogLevel, function: String,
      args: => Any = "")
      (observable: Observable[A])
    : Observable[A] =
      observable
        .doOnSubscribe(
          Task.when(logger.isEnabled(logLevel))(Task(
            logStart(logger, logLevel, function, args))))
        .guaranteeCase(exitCase =>
          Task.when(logger.isEnabled(logLevel))(Task(
            logExitCase(logger, logLevel, function, args, duration = "", exitCase))))
  }

  private final class StartReturnLogContext(logger: ScalaLogger, logLevel: LogLevel,
    function: String, args: => Any = "")
  {
    logStart(logger, logLevel, function, args)
    private val startedAt = System.nanoTime()

    private def duration: String =
      if (startedAt == 0)
        ""
      else
        (System.nanoTime() - startedAt).ns.pretty + " "

    def logExitCase(exitCase: ExitCase[Throwable]): Unit =
      Logger.logExitCase(logger, logLevel, function, "", duration, exitCase)

    def logReturn(marker: String, msg: AnyRef): Unit =
      Logger.logReturn(logger, logLevel, function, "", duration, marker, msg)
  }

  private def logStart(logger: ScalaLogger, logLevel: LogLevel, function: String,
    args: => Any = "")
  : Unit = {
    lazy val argsString = args.toString
    if (argsString.isEmpty)
      logLevel match {
        case LogLevel.LogNone =>
        case LogLevel.Trace => logger.trace(s"↘ $function ↘")
        case LogLevel.Debug => logger.debug(s"↘ $function ↘")
        case LogLevel.Info  => logger.info (s"↘ $function ↘")
        case LogLevel.Warn  => logger.warn (s"↘ $function ↘")
        case LogLevel.Error => logger.error(s"↘ $function ↘")
      }
    else
      logLevel match {
        case LogLevel.LogNone =>
        case LogLevel.Trace => logger.trace(s"↘ $function($argsString) ↘")
        case LogLevel.Debug => logger.debug(s"↘ $function($argsString) ↘")
        case LogLevel.Info  => logger.info (s"↘ $function($argsString) ↘")
        case LogLevel.Warn  => logger.warn (s"↘ $function($argsString) ↘")
        case LogLevel.Error => logger.error(s"↘ $function($argsString) ↘")
      }
  }

  private def logExitCase(
    logger: ScalaLogger,
    logLevel: LogLevel,
    function: String,
    args: => Any,
    duration: String,
    exitCase: ExitCase[Throwable])
  : Unit =
    exitCase match {
      case Error(t) => logReturn(logger, logLevel, function, args, duration, "💥️", t.toStringWithCauses)
      case Canceled => logReturn(logger, logLevel, function, args, duration, "⚫️", "Canceled")
      case Completed => logReturn(logger, logLevel, function, args, duration, "", "Completed")
    }

  private def logReturn(
    logger: ScalaLogger,
    logLevel: LogLevel,
    function: String,
    args: => Any = "",
    duration: String,
    marker: String,
    msg: AnyRef)
  : Unit = {
    lazy val argsString = args.toString
    if (argsString.isEmpty) {
      logLevel match {
        case LogLevel.LogNone =>
        case LogLevel.Trace => logger.trace(s"↙$marker $function => $duration$msg ↙")
        case LogLevel.Debug => logger.debug(s"↙$marker $function => $duration$msg ↙")
        case LogLevel.Info  => logger.info (s"↙$marker $function => $duration$msg ↙")
        case LogLevel.Warn  => logger.warn (s"↙$marker $function => $duration$msg ↙")
        case LogLevel.Error => logger.error(s"↙$marker $function => $duration$msg ↙")
      }
    } else logLevel match {
      case LogLevel.LogNone =>
      case LogLevel.Trace => logger.trace(s"↙$marker $function($argsString) => $duration$msg ↙")
      case LogLevel.Debug => logger.debug(s"↙$marker $function($argsString) => $duration$msg ↙")
      case LogLevel.Info  => logger.info (s"↙$marker $function($argsString) => $duration$msg ↙")
      case LogLevel.Warn  => logger.warn (s"↙$marker $function($argsString) => $duration$msg ↙")
      case LogLevel.Error => logger.error(s"↙$marker $function($argsString) => $duration$msg ↙")
    }
  }
}
