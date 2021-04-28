package js7.base.utils

import cats.data.NonEmptyList
import cats.syntax.option._
import cats.{Functor, Monad, Monoid, Semigroup}
import java.io.{ByteArrayInputStream, InputStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.exceptions.PublicException
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.utils.StackTraces.StackTraceThrowable
import scala.annotation.tailrec
import scala.collection.Factory
import scala.math.max
import scala.reflect.ClassTag
import scala.util.Left
import scala.util.chaining._

object ScalaUtils
{
  private val Ellipsis = "..."
  val RightUnit: Either[Nothing, Unit] = Right(())

  object syntax
  {
    implicit final class RichF_[F[_], A](private val underlying: F[A]) extends AnyVal
    {
      def unless(condition: Boolean)(implicit F: Monoid[F[A]]): F[A] =
        when(!condition)

      def when(condition: Boolean)(implicit F: Monoid[F[A]]): F[A] =
        if (condition)
          underlying
        else
          F.empty
    }

    implicit final class RichEitherF[F[_], L, R](private val underlying: F[Either[L, R]]) extends AnyVal
    {
      def rightAs[R1](newRight: => R1)(implicit F: Functor[F]): F[Either[L, R1]] =
        F.map(underlying)(_.map(_ => newRight))

      def mapt[R1](f: R => R1)(implicit F: Functor[F]): F[Either[L, R1]] =
        F.map(underlying)(_.map(f))

      def mapT[L1 >: L, R1](f: R => Either[L1, R1])(implicit F: Functor[F]): F[Either[L1, R1]] =
        F.map(underlying) {
          case Left(o) => Left(o)
          case Right(o) => f(o)
        }

      /** Simple alernative to `EitherT` `flatMap` if for-comprehension is not needed. */
      def flatMapT[R1](f: R => F[Either[L, R1]])(implicit F: Monad[F]): F[Either[L, R1]] =
        F.flatMap(underlying) {
          case Left(left) => F.pure(Left(left))
          case Right(right) => f(right)
        }
    }

    implicit final class RichEitherIterable[F[x] <: Iterable[x], L, R](private val iterable: F[Either[L, R]])
    extends AnyVal
    {
      /** Combines left sides of any, otherwise return right sides.*/
      def reduceLeftEither(implicit F: Factory[R, F[R]], L: Semigroup[L]): Either[L, F[R]] =
        NonEmptyList.fromList(iterable.view.collect { case Left(l) => l }.toList) match {
          case Some(ls) => Left(ls.reduce)
          case None => Right(iterable.collect { case Right(r) => r }.to(F))
        }
    }

    /** orElse inside a F[Option]. */
    implicit final class RichOptionF[F[_], A](private val underlying: F[Option[A]]) extends AnyVal
    {
      def orElseT(alternative: => F[Option[A]])(implicit F: Monad[F]): F[Option[A]] =
        F.flatMap(underlying) {
          case None => alternative
          case Some(a) => F.pure(a.some)
        }

      /** Simple alternative to `EitherT` `flatMap` if for-comprehension is not needed. */
      def flatMapT(f: A => F[Option[A]])(implicit F: Monad[F]): F[Option[A]] =
        F.flatMap(underlying) {
          case None => F.pure(None)
          case Some(a) => f(a)
        }
    }

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

    implicit final class RichThrowable[A <: Throwable](private val throwable: A) extends AnyVal
    {
      def rootCause: Throwable = {
        @tailrec def cause(t: Throwable): Throwable =
          t.getCause match {
            case null => t
            case o if o == t => t
            case o => cause(o)
          }
        cause(throwable)
      }

      def toStringWithCausesAndStackTrace: String =
        throwable.toStringWithCauses +
          (throwable.getStackTrace.nonEmpty ?? ("\n" + throwable.stackTraceAsString))

      def toStringWithCauses: String = {
        var result = throwable.toSimplifiedString
        throwable.getCause match {
          case null =>
          case cause =>
            val c = cause.toStringWithCauses
            if (!result.contains(c)) {
              result = result.stripSuffix(":") + ", caused by: " + c
            }
        }
        if (throwable.getSuppressed.nonEmpty) {
          result += throwable.getSuppressed.map(t => " [suppressed: " + t.toStringWithCauses + "]").mkString
        }
        result
      }

      def toSimplifiedString: String = {
        val msg = throwable.getMessage
        if (msg != null && msg != "" && (
            throwable.isInstanceOf[ProblemException] ||
            throwable.getClass == classOf[IllegalArgumentException] ||
            throwable.getClass == classOf[RuntimeException] ||
            throwable.getClass == classOf[Exception] ||
            throwable.isInstanceOf[PublicException] ||
            throwable.getClass.getName == "scala.scalajs.js.JavaScriptException" ||
            throwable.getClass.getName == "java.util.concurrent.CompletionException"))
          msg
        else
          throwable match {
            case _: java.util.NoSuchElementException =>
              throwable.toString stripPrefix "java.util."

            case _: IllegalStateException | _: NumberFormatException =>
              throwable.toString stripPrefix "java.lang."

            case _ =>
              throwable.toString
          }
      }.trim

      def stackTraceAsString: String = {
        val w = new StringWriter
        throwable.printStackTrace(new PrintWriter(w))
        w.toString
      }

      /** Useable for logging.
        * `logger.info(throwable.toStringWithCauses, throwable.nullIfNoStackTrace)` */
      def nullIfNoStackTrace: Throwable =
        if (throwable.getStackTrace.isEmpty) null else throwable

      def ifStackTrace: Option[Throwable] =
        Option(nullIfNoStackTrace)

      def dropTopMethodsFromStackTrace(methodName: String): A = {
        val stackTrace = throwable.getStackTrace
        var i = 0
        while (i < stackTrace.length && stackTrace(i).getMethodName == methodName) i += 1
        if (i > 0) throwable.setStackTrace(stackTrace.drop(i))
        throwable
      }
    }

    implicit final class RichAny[A](private val delegate: A) extends AnyVal
    {
      /** Apply the function. */
      @inline def |>[B](f: A => B): B =
        delegate.pipe(f)

      /** Apply the function conditionally. */
      def pipeIf[B >: A](condition: => Boolean)(f: A => B): B =
        if (condition) f(delegate) else delegate

      /** Apply the function conditionally. */
      def pipeMaybe[O, B >: A](maybe: => Option[O])(f: (A, O) => B): B =
        maybe match {
          case Some(o) => f(delegate, o)
          case None => delegate
        }

      def narrow[B <: A: ClassTag]: Checked[B] =
        checkedCast[B](delegate)

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

      def checkNoDuplicate(key: A)(implicit A: ClassTag[A]): Checked[Unit] =
        if (underlying isDefinedAt key)
          Left(DuplicateKey(A.runtimeClass.shortClassName, key))
        else
          RightUnit

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

    private val SomeTrue = Some(true)

    implicit final class RichBoolean(private val underlying: Boolean) extends AnyVal
    {
      /**
        * Conditional `Option`.
        * <p>`(true ? a) == Some(a)`
        * <br>`(false ? a) == None`
        */
      def ?[A](a: => A): Option[A] =
        option(a)

      /** Optional false.
        * <p>`true.? == Some(a)`
        * <br>`false.? == None`
        */
      def ? : Option[Boolean] =
        if (underlying) SomeTrue else None

      /**
        * Conditional `Option`.
        * <p>`(true option a) == Some(a)`
        * <br>`(false option a) == None`
        */
      def option[A](a: => A): Option[A] =
        if (underlying) Some(a) else None

      /**
        * Conditional `List`.
        * <p>`(true option a) == List(a)`
        * <br>`(false option a) == Nil`
        */
      def thenList[A](a: => A): List[A] =
        if (underlying) a :: Nil else Nil

      /**
        * Conditional `Vector`.
        * <p>`(true option a) == Vector(a)`
        * <br>`(false option a) == Vector.empty`
        */
      def thenVector[A](a: => A): Vector[A] =
        if (underlying) Vector(a) else Vector.empty

      /**
        * Conditional `Set`.
        * <p>`(true option a) == Set(a)`
        * <br>`(false option a) == Set.empty`
        */
      def thenSet[A](a: => A): Set[A] =
        if (underlying) Set(a) else Set.empty

      /**
        * Conditional `Iterator`.
        * <p>`(true option a) == Iterator(a)`
        * <br>`(false option a) == Iterator.empty`
        */
      def thenIterator[A](a: => A): Iterator[A] =
        if (underlying) Iterator.single(a) else Iterator.empty

      /** The string on the right side if true, otherwise the empty string. */
      def ??(string: => String): String =
        if (underlying) string else ""

      def !!(problem: => Problem): Checked[Unit] =
        if (!underlying) Left(problem)
        else RightUnit

      def toInt: Int =
        if (underlying) 1 else 0
    }

    implicit final class RichEither[L, R](private val either: Either[L, R]) extends AnyVal
    {
      def rightAs[R1](newRight: => R1): Either[L, R1] =
        either.map(_ => newRight)

      /** Useful for `Checked` to combine both `Problem`s. */
      def combineLeft[R1](other: Either[L, R1])(implicit L: Semigroup[L]): Either[L, (R, R1)] =
        (either, other) match {
          case (Left(a), Left(b)) => Left(L.combine(a, b))
          case (Left(a), Right(_)) => Left(a)
          case (Right(_), Left(b)) => Left(b)
          case (Right(r), Right(r1)) => Right((r, r1))
        }

      def orThrow: R =
        either match {
          case Left(problem: Problem) => throw problem.throwable
            .dropTopMethodsFromStackTrace("orThrow$extension")

          case Left(_) => throw new NoSuchElementException(s"Either.orThrow on $either")
            .dropTopMethodsFromStackTrace("orThrow$extension")

          case Right(r) => r
        }

      def leftOrThrow: L =
        either match {
          case Left(l) => l
          case Right(_) => throw new NoSuchElementException(s"Either.orThrow on $either")
            .dropTopMethodsFromStackTrace("orThrow$extension")
        }
    }

    implicit final class RichThrowableEither[L <: Throwable, R](private val underlying: Either[L, R]) extends AnyVal
    {
      def orThrow: R =
        orThrow(_.dropTopMethodsFromStackTrace("orThrow$extension"))

      def orThrow(toThrowable: L => Throwable): R =
        underlying match {
          case Left(t) => throw toThrowable(t.appendCurrentStackTrace)
          case Right(o) => o
        }

      /** Converts an `Either[Throwable, A]` to a Checked[A] with complete Throwable. */
      def toThrowableChecked: Checked[R] =
        underlying match {
          case Left(t) => Left(Problem.fromThrowable(t.appendCurrentStackTrace))
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

      def dropLastWhile(predicate: Char => Boolean): String = {
        var i = underlying.length
        while (i > 0 && predicate(underlying(i - 1))) i = i -1
        underlying.substring(0, i)
      }
    }

    implicit final class RichByteArray(private val underlying: Array[Byte]) extends AnyVal
    {
      def indexOfByte(byte: Byte) = {
        val len = underlying.length
        var i = 0
        while (i < len && underlying(i) != byte) i = i + 1
        if (i == len) -1 else i
      }
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
      Problem(s"Expected ${implicitClass[A].getName} but got ${o.getClass.getName}: ${o.toString.truncateWithEllipsis(30)}"))

  def checkedCast[A: ClassTag](o: Any, problem: => Problem): Checked[A] = {
    val A = implicitClass[A]
    if (o == null)
      Left(Problem.fromThrowable(new NullPointerException(s"Expected ${A.getName}, found: null")))
    else if (A isAssignableFrom o.getClass)
      Right(o.asInstanceOf[A])
    else
      Left(problem)
  }

  def ifCast[A: ClassTag](o: Any): Option[A] =
    if (o == null)
      None
    else if (implicitClass[A] isAssignableFrom o.getClass)
      Some(o.asInstanceOf[A])
    else
      None

  def someUnless[A](a: A, none: A): Option[A] =
    if (a == none) None else Some(a)

  /** Simple implementation (for tests), converts the string to an Array[Byte],
    * risking `OutOfMemoryError` for long Strings. */
  def shortStringToInputStream(string: String): InputStream =
    new ByteArrayInputStream(string.getBytes(UTF_8))  // OutOfMemoryError

  private val lowerCaseHex: Array[Char] = "0123456789abcdef".toArray

  def bytesToHex(bytes: collection.Seq[Byte]): String = {
    val sb = new StringBuilder(2 * bytes.length)
    for (b <- bytes.iterator) {
      sb.append(lowerCaseHex((b >> 4) & 0xf))
      sb.append(lowerCaseHex(b & 0xf))
    }
    sb.toString
  }
}
