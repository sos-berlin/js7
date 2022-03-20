package js7.base.log

import cats.effect.ExitCase.{Canceled, Completed, Error}
import com.typesafe.scalalogging.{Logger => ScalaLogger}
import js7.base.problem.Problem
import js7.base.time.ScalaTime.{DurationRichLong, RichDuration}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.StackTraces.StackTraceThrowable
import monix.eval.Task
import org.slf4j.{LoggerFactory, Marker, MarkerFactory}
import scala.reflect.ClassTag

object Logger
{
  Slf4jUtils.initialize()

  //val Timing: Marker = MarkerFactory.getMarker("Timing")
  //val Event: Marker = MarkerFactory.getMarker("Event")
  val Actor: Marker = MarkerFactory.getMarker("Actor")
  val Java: Marker = MarkerFactory.getMarker("Java")
  //val Repo: Marker = MarkerFactory.getMarker("Repo")
  val SignatureVerified: Marker = MarkerFactory.getMarker("SignatureVerified")

  def apply[A: ClassTag]: ScalaLogger =
    apply(implicitClass[A])

  def apply(c: Class[_]): ScalaLogger =
    ScalaLogger(normalizeClassName(c))

  def apply(name: String): ScalaLogger =
    ScalaLogger(name)

  def withPrefix[A: ClassTag](prefix: String): ScalaLogger =
    withPrefix(implicitClass[A], prefix)

  def withPrefix(c: Class[_], prefix: String): ScalaLogger =
    if (prefix.isEmpty)
      apply(c)
    else
      ScalaLogger(new ConvertingLogger.Prefixed(
        prefix,
        LoggerFactory.getLogger(normalizeClassName(c))))

  /** Removes '$' from Scala's companion object class. */
  def normalizeClassName(c: Class[_]): String =
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

      def debugTask[A](function: String, args: => Any = "")(task: Task[A]): Task[A] =
        logTask(logger, function, args)(task)

      def traceTask[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        traceTask(src.value)(task)

      def traceTask[A](function: String, args: => Any = "")(task: Task[A]): Task[A] =
        logTask(logger, function, args, trace = true)(task)
    }

    private def logTask[A](logger: ScalaLogger, function: String, args: => Any = "",
      trace: Boolean = false)(task: Task[A])
    : Task[A] =
      Task.defer {
        if (trace && !logger.underlying.isTraceEnabled || !logger.underlying.isDebugEnabled)
          task
        else {
          val argsString = args.toString
          // Are these optimizations justified ???
          if (argsString.isEmpty) {
            if (trace) {
              logger.trace(s"↘︎ $function ...")
            } else {
              logger.debug(s"↘︎ $function ...")
            }
          } else
            if (trace) {
              logger.trace(s"↘︎ $function($argsString) ...")
            } else {
              logger.debug(s"↘︎ $function($argsString) ...")
            }

          val t = System.nanoTime()

          def logReturn(marker: String, msg: AnyRef) =
            Task {
              val duration = if (t == 0) "" else (System.nanoTime() - t).ns.pretty + " "
              if (argsString.isEmpty) {
                if (trace) {
                  logger.trace(s"︎↙$marker $function => $duration$msg")
                } else {
                  logger.debug(s"︎↙$marker $function => $duration$msg")
                }
              } else
                if (trace) {
                  logger.trace(s"︎↙$marker $function($argsString) => $duration$msg")
                } else {
                  logger.debug(s"︎↙$marker $function($argsString) => $duration$msg")
                }
            }

          task
            .tapEval {
              case left @ Left(_: Throwable | _: Problem) =>
                logReturn("🚫", left)
              case _ =>
                logReturn("", "Completed")
            }
            .guaranteeCase {
              case Completed => Task.unit
              case Error(t) => logReturn("💥️", t.toStringWithCauses)
              case Canceled => logReturn("❌", Canceled)
            }
        }
      }
  }
}
