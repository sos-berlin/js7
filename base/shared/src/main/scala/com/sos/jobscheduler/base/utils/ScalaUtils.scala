package com.sos.jobscheduler.base.utils

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.exceptions.PublicException
import com.sos.jobscheduler.base.problem.{Checked, Problem, ProblemException}
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import java.io.{ByteArrayInputStream, InputStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Future
import scala.math.max
import scala.reflect.ClassTag

object ScalaUtils
{
  @inline
  def reuseIfEqual[A <: AnyRef](a: A)(f: A => A): A =
    reuseIfEqual(a, f(a))

  @inline
  def reuseIfEqual[A <: AnyRef](a: A, b: A): A =
    if (a == b) a else b

  def implicitClass[A : ClassTag]: Class[A] = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]

  implicit final class RichJavaClass[A](private val underlying: Class[A]) {
    def scalaName: String = underlying.getName stripSuffix "$"

    def simpleScalaName: String = simpleName stripSuffix "$"

    /**
      * Workaround for JDK-8057919 Class#getSimpleName (resolved in Java 9).
      * @see https://bugs.openjdk.java.net/browse/JDK-8057919
      * @see https://issues.scala-lang.org/browse/SI-2034
      */
    def simpleName: String =
      simpleClassName(underlying.getName)
  }

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

  object implicits {
    implicit final class ToStringFunction1[A, R](private val delegate: A => R) {
      def withToString(string: String): A => R =
        new (A => R) {
          def apply(a: A) = delegate(a)
          override def toString() = string
        }
    }
  }

  def namedIdentity[A] = new (A => A) {
    def apply(a: A) = a
    override def toString = "identity"
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
        (if (delegate.getStackTrace.isEmpty) "" else "\n" + delegate.stackTraceAsString)

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
  }

  def cast[A: ClassTag](o: Any): A = {
    val a = implicitClass[A]
    if (o == null) throw new NullPointerException(s"Expected ${a.getName}, found: null")
    if (!(a isAssignableFrom o.getClass)) throw new ClassCastException(s"${o.getClass.getName} is not a ${a.getName}: $o")
    o.asInstanceOf[A]
  }

  def someUnless[A](a: A, none: A): Option[A] =
    if (a == none) None else Some(a)

  implicit final class RichAny[A](private val delegate: A) extends AnyVal {
    def substitute(substitution: (A, A)): A = substitute(substitution._1, substitution._2)

    @inline def substitute(when: A, _then: => A): A =
      if (delegate == when) _then else delegate
  }

  implicit final class SwitchOnAtomicBoolean(private val delegate: AtomicBoolean) extends AnyVal {
    def switchOn(body: => Unit) = if (delegate.compareAndSet(false, true)) body
    def switchOff(body: => Unit) = if (delegate.compareAndSet(true, false)) body
  }

  implicit final class RichPartialFunction[A, B](private val underlying: PartialFunction[A, B]) extends AnyVal {
    def checked(key: A): Checked[B] =
      toChecked(key)

    def checked(key: A, notFound: => Problem): Checked[B] =
      toChecked_(_ => notFound)(key)

    def toChecked: A => Checked[B] =
      toChecked_(key => Problem(s"No such key '$key'"))

    private def toChecked_(notFound: A => Problem): A => Checked[B] = {
      val lifted = underlying.lift
      key => lifted(key) match {
        case Some(b) => Valid(b)
        case None => Invalid(notFound(key))
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

  implicit final class RichEither[L <: Throwable, R](private val underlying: Either[L, R]) extends AnyVal {
    def toImmediateFuture: Future[R] =
      withStackTrace match {
        case Left(t) => Future.failed(t)
        case Right(o) => Future.successful(o)
      }

    def orThrow: R =
      underlying match {
        case Left(t) => throw t.appendCurrentStackTrace
        case Right(o) => o
      }

    /** Converts an `Either[Throwable, A]` to a Checked[A] with complete Throwable. */
    def toChecked: Checked[R] =
      underlying match {
        case Left(t) => Invalid(Problem.pure(t.appendCurrentStackTrace))
        case Right(o) => Valid(o)
      }

    /** Converts an `Either[Throwable, A]` to a Checked[A] with the `Throwable`'s message only (toStringWithCauses). */
    def toSimpleChecked: Checked[R] =
      underlying match {
        case Left(t) => Invalid(Problem.pure(Option(t.getMessage) getOrElse t.toStringWithCauses))
        case Right(o) => Valid(o)
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

  implicit final class RichValidated[E <: Throwable, A](private val underlying: Validated[E, A]) extends AnyVal {
    def orThrow: A = underlying.valueOr(t => throw t.appendCurrentStackTrace)
  }

  /** Simple implementation (for tests), converts the string to an Array[Byte],
    * risking `OutOfMemoryError` for long Strings. */
  def shortStringToInputStream(string: String): InputStream =
    new ByteArrayInputStream(string.getBytes(UTF_8))  // OutOfMemoryError
}
