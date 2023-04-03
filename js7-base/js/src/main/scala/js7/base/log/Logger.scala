package js7.base.log

import cats.effect.Resource
import js7.base.utils.ScalaUtils.implicitClass
import monix.eval.Task
import monix.reactive.Observable
import scala.reflect.ClassTag

object Logger
{
  val empty: scribe.Logger =
    scribe.Logger.empty

  def initialize() = {}

  def apply[A: ClassTag]: scribe.Logger =
    apply(implicitClass[A])

  def apply(c: Class[?]): scribe.Logger =
    scribe.Logger(normalizeClassName(c))

  def apply(name: String): scribe.Logger =
    scribe.Logger(name)

  /** Removes '$' from Scala's companion object class. */
  def normalizeClassName(c: Class[?]): String =
    c.getName stripSuffix "$"

  object syntax {
    // Empty implementation to be compatible with the JVM variant
    implicit final class RichLogger(private val logger: scribe.Logger) extends AnyVal {
      def debugTask[A](task: Task[A])/*(implicit src: sourcecode.Name)*/: Task[A] =
        task

      def debugTask[A](functionName: String, args: => Any = "")(task: Task[A]): Task[A] =
        task

      def debugTaskWithResult[A](task: Task[A])/*(implicit src: sourcecode.Name)*/: Task[A] =
        task

      def debugTaskWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A](_))
        (task: Task[A])
      : Task[A] =
        task

      def traceTask[A](task: Task[A])/*(implicit src: sourcecode.Name)*/: Task[A] =
        task

      def traceTask[A](function: String, args: => Any = "")(task: Task[A]): Task[A] =
        task

      def traceTaskWithResult[A](task: Task[A])(implicit src: sourcecode.Name): Task[A] =
        traceTaskWithResult[A](src.value)(task)

      def traceTaskWithResult[A](
        function: String,
        args: => Any = "",
        result: A => Any = identity[A](_))
        (task: Task[A])
      : Task[A] =
        task

      def debugResource[A](resource: Resource[Task, A])(implicit src: sourcecode.Name)
      : Resource[Task, A] =
        debugResource(src.value)(resource)

      def debugResource[A](function: String, args: => Any = "")(resource: Resource[Task, A])
      : Resource[Task, A] =
        resource

      def debugObservable[A](observable: Observable[A])/*(implicit src: sourcecode.Name)*/
      : Observable[A] =
        observable

      def debugObservable[A](function: String, args: => Any = "")(observable: Observable[A])
      : Observable[A] =
        observable
    }
  }
}
