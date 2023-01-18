package js7.base.log

import cats.Applicative
import cats.effect.ExitCase.{Canceled, Completed, Error}
import cats.effect.{ExitCase, Resource, Sync}
import com.typesafe.scalalogging.Logger as ScalaLogger
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

  val empty: ScalaLogger =
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
  def normalizeClassName(c: Class[?]): String =
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
      def debugTask[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        debugTask(src.value)(task)

      def debugTask[A](functionName: String, args: => Any = "")(task: Task[A]): Task[A] =
        logTask[A](logger, functionName, args)(task)

      def debugTaskWithResult[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        debugTaskWithResult[A](src.value)(task)

      def debugTaskWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A](_))
        (task: Task[A])
      : Task[A] =
        logTask[A](logger, function, args, result)(task)

      def traceTask[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        traceTask(src.value)(task)

      def traceTask[A](function: String, args: => Any = "")(task: Task[A]): Task[A] =
        logTask[A](logger, function, args, trace = true)(task)

      def traceTaskWithResult[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        traceTaskWithResult[A](src.value)(task)

      def traceTaskWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A](_))
        (task: Task[A])
      : Task[A] =
        logTask[A](logger, function, args, result, trace = true)(task)

      def debugResource[A](resource: Resource[Task, A])(implicit src: sourcecode.Name)
      : Resource[Task, A] =
        debugResource(src.value)(resource)

      def debugResource[A](function: String, args: => Any = "")(resource: Resource[Task, A])
      : Resource[Task, A] =
        logResource[Task, A](logger, function, args)(resource)

      def debugObservable[A](observable: Observable[A])(implicit src: sourcecode.Name)
      : Observable[A] =
        debugObservable(src.value + ": Observable")(observable)

      def debugObservable[A](function: String, args: => Any = "")(observable: Observable[A])
      : Observable[A] =
        logObservable[A](logger, function, args)(observable)
    }

    private def logTask[A](logger: ScalaLogger, function: String, args: => Any = "",
      resultToLoggable: A => Any = null,
      trace: Boolean = false)
      (task: Task[A])
    : Task[A] =
      Task.defer {
        if (!isLoggingEnabled(logger, trace))
          task
        else {
          val ctx = new StartReturnLogContext(logger, function, args, trace)
          task
            .tapEval {
              case left @ Left(_: Throwable | _: Problem) =>
                Task(ctx.logReturn("❓", left))
              case result =>
                Task(ctx.logReturn(
                  "",
                  if (resultToLoggable eq null)
                    "Completed"
                  else
                    resultToLoggable(result).toString))
            }
            .guaranteeCase {
              case Completed => Task.unit
              case exitCase => Task(ctx.logExitCase(exitCase))
            }
        }
      }

    private def logResource[F[_], A](
      logger: ScalaLogger,
      function: String,
      args: => Any = "",
      trace: Boolean = false)
      (resource: Resource[F, A])
      (implicit F: Applicative[F] & Sync[F])
    : Resource[F, A] =
      Resource
        .makeCase(
          acquire = F.delay(
            if (!isLoggingEnabled(logger, trace))
              None
            else
              Some(new StartReturnLogContext(logger, function, args, trace))))(
          release = (maybeCtx, exitCase) =>
            F.delay(
              for (ctx <- maybeCtx) ctx.logExitCase(exitCase)))
        .flatMap(_ => resource)

    private def logObservable[A](
      logger: ScalaLogger,
      function: String,
      args: => Any = "",
      trace: Boolean = false)
      (observable: Observable[A])
    : Observable[A] =
      observable
        .doOnSubscribe(
          Task.when(isLoggingEnabled(logger, trace))(Task(
            logStart(logger, function, args, trace))))
        .guaranteeCase(exitCase =>
          Task.when(isLoggingEnabled(logger, trace))(Task(
            logExitCase(logger, function, args, trace, duration = "", exitCase))))
  }

  private final class StartReturnLogContext(
    logger: ScalaLogger,
    function: String,
    args: => Any = "",
    trace: Boolean = false)
  {
    logStart(logger, function, args, trace)
    private val startedAt = System.nanoTime()

    private def duration: String =
      if (startedAt == 0)
        ""
      else
        (System.nanoTime() - startedAt).ns.pretty + " "

    def logExitCase(exitCase: ExitCase[Throwable]) =
      Logger.logExitCase(logger, function, args, trace, duration, exitCase)

    def logReturn(marker: String, msg: AnyRef) =
      Logger.logReturn(logger, function, args, trace, duration, marker, msg)
  }

  private def logStart(
    logger: ScalaLogger,
    function: String,
    args: => Any = "",
    trace: Boolean = false)
  : Unit = {
    // Are these optimizations justified ???
    lazy val argsString = args.toString
    if (argsString.isEmpty) {
      if (trace) {
        logger.trace(s"↘ $function ...")
      } else {
        logger.debug(s"↘ $function ...")
      }
    } else {
      if (trace) {
        logger.trace(s"↘ $function($argsString) ...")
      } else {
        logger.debug(s"↘ $function($argsString) ...")
      }
    }
  }

  private def logExitCase(
    logger: ScalaLogger,
    function: String,
    args: => Any = "",
    trace: Boolean = false,
    duration: String,
    exitCase: ExitCase[Throwable])
  : Unit =
    exitCase match {
      case Error(t) => logReturn(logger, function, args, trace, duration, "💥️", t.toStringWithCauses)
      case Canceled => logReturn(logger, function, args, trace, duration, "❌", Canceled)
      case Completed => logReturn(logger, function, args, trace, duration, "", "Completed")
    }

  private def logReturn(
    logger: ScalaLogger,
    function: String,
    args: => Any = "",
    trace: Boolean = false,
    duration: String,
    marker: String,
    msg: AnyRef)
  : Unit = {
    lazy val argsString = args.toString
    if (argsString.isEmpty) {
      if (trace) {
        logger.trace(s"↙$marker $function => $duration$msg")
      } else {
        logger.debug(s"↙$marker $function => $duration$msg")
      }
    } else if (trace) {
      logger.trace(s"↙$marker $function($argsString) => $duration$msg")
    } else {
      logger.debug(s"↙$marker $function($argsString) => $duration$msg")
    }
  }

  private def isLoggingEnabled(logger: ScalaLogger, trace: Boolean) =
    if (trace)
      logger.underlying.isTraceEnabled
    else
      logger.underlying.isDebugEnabled
}
