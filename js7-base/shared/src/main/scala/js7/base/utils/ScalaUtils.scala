package js7.base.utils

import java.io.{ByteArrayInputStream, InputStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.exceptions.PublicException
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.utils.StackTraces.StackTraceThrowable
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Future
import scala.math.max
import scala.reflect.ClassTag
import scala.util.chaining._

object ScalaUtils
{
  private val Ellipsis = "..."

  object syntax
  {
    implicit final class RichJavaClass[A](private val underlying: Class[A])
    {
      def scalaName: String = underlying.getName stripSuffix "$"

      def simpleScalaName: String = simpleName stripSuffix "$"

      /**
        * Workaround for JDK-8057919 Class#getSimpleName (resolved in Java 9).
        * @see https://bugs.openjdk.java.net/browse/JDK-8057919
        * @see https://issues.scala-lang.org/browse/SI-2034
        */
      def simpleName: String =
        simpleClassName(underlying.getName)

      /** Simple class name prefixed by maybe outer class name. */
      def shortClassName: String =
        if (underlying.isPrimitive)
          underlying.getName.capitalize
        else {
          // Change '$' inner class concationation character to '.'
          val simpleName = simpleScalaName
          val prefix = scalaName stripSuffix simpleName
          val name = (if (prefix endsWith "$") prefix.init + '.' else prefix) + simpleName
          removePackageRegex.replaceFirstIn(name, "")
        }
    }

    implicit final class ToStringFunction1[A, R](private val delegate: A => R) {
      def withToString(string: String): A => R =
        new (A => R) {
          def apply(a: A) = delegate(a)
          override def toString() = string
        }
    }

    implicit final class RichThrowable[A <: Throwable](private val delegate: A) extends AnyVal {
      def rootCause: Throwable = {
        @tailrec def cause(t: Throwable): Throwable =
          t.getCause match {
            case null => t
            case o if o == t => t
            case o => cause(o)
          }
        cause(delegate)
      }

      def toStringWithCausesAndStackTrace: String =
        delegate.toStringWithCauses +
          (delegate.getStackTrace.nonEmpty ?: ("\n" + delegate.stackTraceAsString))

      def toStringWithCauses: String = {
        val strings = mutable.Buffer[String]()
        var t: Throwable = delegate
        while (t != null) {
          strings += t.toSimplifiedString.trim.stripSuffix(":")
          t = t.getCause
        }
        strings mkString ", caused by: "
      }

      def toSimplifiedString: String = {
        val msg = delegate.getMessage
        if (msg != null && msg != "" && (
            delegate.isInstanceOf[ProblemException] ||
            delegate.getClass == classOf[IllegalArgumentException] ||
            delegate.getClass == classOf[RuntimeException] ||
            delegate.isInstanceOf[PublicException] ||
            delegate.getClass.getName == "scala.scalajs.js.JavaScriptException"))
          msg
        else
          delegate match {
            case _: java.util.NoSuchElementException =>
              delegate.toString stripPrefix "java.util."

            case _: IllegalStateException | _: NumberFormatException =>
              delegate.toString stripPrefix "java.lang."

            case _ =>
              delegate.toString
          }
      }

      def stackTraceAsString: String = {
        val w = new StringWriter
        delegate.printStackTrace(new PrintWriter(w))
        w.toString
      }

      /** Useable for logging.
        * `logger.info(throwable.toStringWithCauses, throwable.nullIfNoStackTrace)` */
      def nullIfNoStackTrace: Throwable =
        if (delegate.getStackTrace.isEmpty) null else delegate

      def ifNoStackTrace: Option[Throwable] =
        Option(nullIfNoStackTrace)
    }

    implicit final class RichAny[A](private val delegate: A) extends AnyVal
    {
      /** Apply the function. */
      @inline def |>[B](f: A => B): B =
        delegate.pipe(f)

      /** Apply the function conditionally. */
      def pipeIf[B >: A](condition: => Boolean, f: A => B): B =
        if (condition) f(delegate) else delegate

      def substitute(substitution: (A, A)): A =
        substitute(substitution._1, substitution._2)

      @inline def substitute(when: A, _then: => A): A =
        if (delegate == when) _then else delegate
    }

    implicit final class SwitchOnAtomicBoolean(private val delegate: AtomicBoolean) extends AnyVal {
      def switchOn(body: => Unit) = if (delegate.compareAndSet(false, true)) body
      def switchOff(body: => Unit) = if (delegate.compareAndSet(true, false)) body
    }

    implicit final class RichPartialFunction[A, B](private val underlying: PartialFunction[A, B])
    extends AnyVal
    {
      def checked(key: A)(implicit A: ClassTag[A]): Checked[B] =
        rightOr(key, UnknownKeyProblem(A.runtimeClass.shortClassName, key))

      def rightOr(key: A, notFound: => Problem): Checked[B] =
        toChecked(_ => notFound)(key)

      private def toChecked(notFound: A => Problem): A => Checked[B] = {
        val lifted = underlying.lift
        key => lifted(key) match {
          case Some(b) => Right(b)
          case None => Left(notFound(key))
        }
      }

      def checkNoDuplicate(key: A)(implicit A: ClassTag[A]): Checked[Unit] = {
        val lifted = underlying.lift
        lifted(key) match {
          case Some(_) => Left(DuplicateKey(A.runtimeClass.shortClassName, key))
          case None => Right(())
        }
      }

      def getOrElse[BB >: B](key: A, default: => BB): BB =
        underlying.applyOrElse(key, (_: A) => default)

      /** applyOrElse calls isDefined, not optimized. */
      def map[C](f: B => C): PartialFunction[A, C] =
        mapPartialFunction(f)

      /** applyOrElse calls isDefined, not optimized. */
      def mapPartialFunction[C](f: B => C): PartialFunction[A, C] = {
        case o if underlying.isDefinedAt(o) => f(underlying(o))
      }
    }

    implicit final class RichUnitPartialFunction[A](private val delegate: PartialFunction[A, Unit]) extends AnyVal {
      def callIfDefined(a: A): Unit = delegate.getOrElse(a, ())
    }

    implicit final class SwitchStatement[A](private val delegate: A) extends AnyVal {
      def switch[B](pf: PartialFunction[A, Unit]): Unit = pf.callIfDefined(delegate)
    }

    implicit final class RichOption[A](val underlying: Option[A]) extends AnyVal {
      def whenEmpty(f: => Unit): underlying.type = {
        if (underlying.isEmpty) {
          f
        }
        underlying
      }
    }

    implicit final class RichEither[L, R](private val underlying: Either[L, R]) extends AnyVal
    {
      // Will be no longer needed with Scala 2.13 !!!
      def orElse[L1 >: L, R1 >: R](or: => Either[L1, R1]): Either[L1, R1] =
        underlying match {
          case Left(_) => or
          case Right(o) => Right(o)
        }
    }

    implicit final class RichThrowableEither[L <: Throwable, R](private val underlying: Either[L, R]) extends AnyVal {
      def toFuture: Future[R] =
        withStackTrace match {
          case Left(t) => Future.failed(t)
          case Right(o) => Future.successful(o)
        }

      def orThrow: R =
        orThrow(identity)

      def orThrow(toThrowable: L => Throwable): R =
        underlying match {
          case Left(t) => throw toThrowable(t).appendCurrentStackTrace
          case Right(o) => o
        }

      /** Converts an `Either[Throwable, A]` to a Checked[A] with complete Throwable. */
      def toThrowableChecked: Checked[R] =
        underlying match {
          case Left(t) => Left(Problem.pure(t.appendCurrentStackTrace))
          case Right(o) => Right(o)
        }

      /** Converts an `Either[Throwable, A]` to a Checked[A] with the `Throwable`'s message only (toStringWithCauses). */
      def toMessageOnlyChecked: Checked[R] =
        underlying match {
          case Left(t) => Left(Problem.pure(Option(t.getMessage) getOrElse t.toStringWithCauses))
          case Right(o) => Right(o)
        }

      def withStackTrace: Either[Throwable, R] =
        underlying match {
          case o: Right[L, R] =>
            o

          case Left(t) if t.getStackTrace.nonEmpty =>
            Left(t)

          case Left(t) =>
            t.fillInStackTrace()
            Left(if (t.getStackTrace.nonEmpty) t else new IllegalStateException(s"$t", t))
        }
    }

    implicit final class RichString(private val underlying: String) extends AnyVal
    {
      /** Truncate to `n`, replacing the tail with ellipsis and, if the string is long, the total character count. */
      def truncateWithEllipsis(n: Int, showLength: Boolean = false): String = {
        val suffix = if (showLength) s"$Ellipsis(length ${underlying.length})" else Ellipsis
        val nn = max(suffix.length, n)
        if (underlying.lengthIs <= nn)
          underlying
        else
          underlying.take(nn - suffix.length) + suffix
      }

      def replaceChar(from: Char, to: Char): String =
        if (underlying contains from) {
          val chars = new Array[Char](underlying.length)
          underlying.getChars(0, underlying.length, chars, 0)
          for (i <- chars.indices) if (chars(i) == from) chars(i) = to
          new String(chars)
        } else
          underlying

      def reverseDropWhile(predicate: Char => Boolean): String = {
        var i = underlying.length
        while (i > 0 && predicate(underlying(i - 1))) i = i -1
        underlying.substring(0, i)
      }

      @inline def ?:(condition: Boolean): String =
        when(condition)

      @inline def when(condition: Boolean): String =
        if (condition) underlying else ""
    }
  }
  import syntax._

  @inline
  def reuseIfEqual[A <: AnyRef](a: A)(f: A => A): A =
    reuseIfEqual(a, f(a))

  @inline
  def reuseIfEqual[A <: AnyRef](a: A, b: A): A =
    if (a == b) a else b

  def implicitClass[A : ClassTag]: Class[A] = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]

  private val removePackageRegex = """^([a-z0-9]*\.)*""".r

  private[utils] def simpleClassName[A](name: String): String = {
    name.substring(max(name.lastIndexOf('.', name.length - 2), name.lastIndexOf('$', name.length - 2)) + 1)
  }

  /**
    * Replaces toString of the body (argumentless function), for logging and debugging.
    */
  def function0WithToString[R](lazyString: => String)(body: => R): () => R =
    new (() => R) {
      def apply() = body
      override def toString() = lazyString
    }

  /**
   * Replaces toString of the function, for logging and debugging.
   */
  def function1WithToString[A, R](string: String)(function: A => R): A => R =
    new (A => R) {
      def apply(a: A) = function(a)
      override def toString() = string
    }

  def namedIdentity[A] = new (A => A) {
    def apply(a: A) = a
    override def toString = "identity"
  }

  def cast[A: ClassTag](o: Any): A = {
    checkedCast[A](o) match {
      case Left(problem) =>
        throw problem.throwableOption getOrElse new ClassCastException(problem.toString)
      case Right(a) => a
    }
  }

  def checkedCast[A: ClassTag](o: Any): Checked[A] =
    checkedCast[A](o,
      Problem(s"Expected ${o.getClass.getName} but got ${implicitClass[A].getName}: ${o.toString.truncateWithEllipsis(30)}"))

  def checkedCast[A: ClassTag](o: Any, problem: => Problem): Checked[A] = {
    val A = implicitClass[A]
    if (o == null)
      Left(Problem.pure(new NullPointerException(s"Expected ${A.getName}, found: null")))
    else if (A isAssignableFrom o.getClass)
      Right(o.asInstanceOf[A])
    else
      Left(problem)
  }

  def someUnless[A](a: A, none: A): Option[A] =
    if (a == none) None else Some(a)

  /** Simple implementation (for tests), converts the string to an Array[Byte],
    * risking `OutOfMemoryError` for long Strings. */
  def shortStringToInputStream(string: String): InputStream =
    new ByteArrayInputStream(string.getBytes(UTF_8))  // OutOfMemoryError
}
