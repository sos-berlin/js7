package js7.base.log

import cats.effect.ExitCase.{Canceled, Completed, Error}
import com.typesafe.scalalogging.{Logger => ScalaLogger}
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
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

      def debugTask[A](function: String, args: String = "")(task: Task[A]): Task[A] = {
        def logReturn(msg: AnyRef) = Task(logger.debug(s"︎↙︎ $function => $msg"))
        Task.defer {
          logger.debug(s"↘︎ $function${args.emptyToNone.fold("")(" " + _)} ...")
          task.tapEval {
            case left: Left[_, _] => logReturn(left)
            case _ => logReturn("Completed")
          }
        }.guaranteeCase {
            case Completed => Task.unit
            case Error(t) => logReturn(t.toStringWithCauses)
            case Canceled => logReturn(Canceled)
        }
      }
    }
  }
}
