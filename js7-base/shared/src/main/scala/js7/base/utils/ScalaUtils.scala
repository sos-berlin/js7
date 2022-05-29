package js7.base.utils

import cats.syntax.foldable._
import cats.syntax.option._
import cats.{Functor, Monad, Monoid, Semigroup}
import java.io.{ByteArrayInputStream, InputStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.exceptions.PublicException
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.base.utils.StackTraces.StackTraceThrowable
import scala.annotation.tailrec
import scala.collection.{Factory, MapView, View, mutable}
import scala.math.max
import scala.reflect.ClassTag
import scala.util.chaining._

object ScalaUtils
{
  private val Ellipsis = "..."
  val RightUnit: Either[Nothing, Unit] = Right(())
  private val spaceArray = (" " * 64).toCharArray

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

      def rightAs(unit: Unit)(implicit F: Functor[F]): F[Either[L, Unit]] =
        F.map(underlying) {
          case Right(_) => RightUnit
          case o => o.asInstanceOf[Either[L, Unit]]
        }

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
      def reduceLeftEither(implicit F: Factory[R, F[R]], L: Semigroup[L]): Either[L, F[R]] = {
        L.combineAllOption(iterable.view.collect { case Left(l) => l })
          .match_ {
            case Some(problem) => Left(problem)
            case None => Right(iterable.view.collect { case Right(r) => r }.to(F))
          }
      }
    }

    implicit final class RichLeftProjection[L, R](private val leftProjection: Either.LeftProjection[L, R])
    extends AnyVal
    {
      @throws[NoSuchElementException]
      def orThrow: L =
        leftProjection.e match {
          case Left(a) => a
          case _       => throw new NoSuchElementException(s"Either.left.get on: ${leftProjection.e}")
        }
    }

    implicit final class RichOption[A](private val option: Option[A]) extends AnyVal
    {
      /** Like Scala's `fold`, but with proper result type derivation. */
      def fold_[B](ifNone: => B, ifSome: A => B): B =
        option match {
          case None => ifNone
          case Some(a) => ifSome(a)
        }

      def !!(problem: => Problem): Checked[A] =
        option match {
          case None => Left(problem)
          case Some(a) => Right(a)
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

      /** match operator as method (like in Scala 3?). */
      def match_[B](f: A => B): B =
        delegate.pipe(f)

      @inline def substitute(when: A, _then: => A): A =
        if (delegate == when) _then else delegate
    }

    implicit final class SwitchOnAtomicBoolean(private val delegate: AtomicBoolean) extends AnyVal {
      def switchOn(body: => Unit) = if (delegate.compareAndSet(false, true)) body
      def switchOff(body: => Unit) = if (delegate.compareAndSet(true, false)) body
    }

    implicit final class RichView[A](private val view: View[A]) extends AnyVal
    {
      def :+[B >: A](b: B): View[B] =
        view.concat(b :: Nil)

      def +:[B >: A](b: B): View[B] =
        new View.Single(b) ++ view
    }

    implicit final class RichScalaUtilsMap[K, V](private val underlying: Map[K, V])
    extends AnyVal
    {
      def checked(key: K)(implicit A: ClassTag[K]): Checked[V] =
        rightOr(key, UnknownKeyProblem(A.runtimeClass.shortClassName, key))

      def rightOr(key: K, notFound: => Problem): Checked[V] =
        underlying.get(key) match {
          case None => Left(notFound)
          case Some(a) => Right(a)
        }
    }

    implicit final class RichMapView[K, V](private val mapView: MapView[K, V])
    extends AnyVal
    {
      def collectValues[V1](pf: PartialFunction[V, V1]): MapView[K, V1] =
        mapView
          .mapValues(pf.lift)
          .filter(_._2.isDefined)
          .mapValues(_.get)

      /** concat reversly, left side has overrides right side. */
      def orElseMapView[V1 >: V](other: MapView[K, V1]): MapView[K, V1] =
        new MapView[K, V1] {
          def get(key: K) = mapView.get(key) orElse other.get(key)

          def iterator =
            mapView.iterator ++ other.filterKeys(k => !mapView.contains(k))
        }
    }

    // Like PartialFunction.Lifted:
    private val fallback_fn: Any => Any = _ => fallback_fn
    private def checkFallback[B] = fallback_fn.asInstanceOf[Any => B]
    private def fallbackOccurred[B](x: B) = fallback_fn eq x.asInstanceOf[AnyRef]

    implicit final class RichPartialFunction[A, B](private val underlying: PartialFunction[A, B])
    extends AnyVal
    {
      def get(key: A): Option[B] = {
        val b = underlying.applyOrElse(key, checkFallback[B])
        if (fallbackOccurred(b)) None else Some(b)
      }

      def checked(key: A)(implicit A: ClassTag[A]): Checked[B] =
        underlying.lift(key) match {
          case None => Left(UnknownKeyProblem(A.runtimeClass.shortClassName, key))
          case Some(a) => Right(a)
        }

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

      def map[C](f: B => C): PartialFunction[A, C] =
        mapPartialFunction(f)

      def mapPartialFunction[C](bToC: B => C): PartialFunction[A, C] =
        new PartialFunction[A, C] {
          def isDefinedAt(a: A) = underlying.isDefinedAt(a)

          def apply(a: A) = bToC(underlying.apply(a))

          override def applyOrElse[A1 <: A, C1 >: C](a: A1, default: A1 => C1): C1 = {
            val b = underlying.applyOrElse(a, checkFallback[B])
            if (fallbackOccurred(b))
              default(a)
            else
              bToC(b)
          }
        }
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

      def option[A](a: => A): Option[A] =
        thenSome(a)

      /**
        * Conditional `Option`.
        * <p>`(true option a) == Some(a)`
        * <br>`(false option a) == None`
        */
      def thenSome[A](a: => A): Option[A] =
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

      /**
        * Conditional `View`.
        * <p>`(true option a) == View.Single(a)`
        * <br>`(false option a) == View.empty`
        */
      def thenView[A](a: => A): View[A] =
        if (underlying) new View.Single(a) else View.empty

      /** The string on the right side if true, otherwise the empty string. */
      def ??(string: => String): String =
        if (underlying) string else ""

      def !!(problem: => Problem): Checked[Unit] =
        orLeft(problem)

      def orLeft[L](left: => L): Either[L, Unit] =
        if (!underlying) Left(left)
        else RightUnit

      def toInt: Int =
        if (underlying) 1 else 0
    }

    implicit final class RichEither[L, R](private val either: Either[L, R]) extends AnyVal
    {
      def rightAs[R1](newRight: => R1): Either[L, R1] =
        either.map(_ => newRight)

      def rightAs(right: Unit): Either[L, Unit] =
        either match {
          case Right(_) => RightUnit
          case _ => either.asInstanceOf[Either[L, Unit]]
        }

      //def flatMapLeft[L1](f: L => Either[L1, R]): Either[L1, R] =
      //  either match {
      //    case Left(o) => f(o)
      //    case o => o.asInstanceOf[Either[L1, R]]
      //  }

      /** Useful for `Checked` to combine both `Problem`s. */
      def combineLeft[R1](other: Either[L, R1])(implicit L: Semigroup[L]): Either[L, (R, R1)] =
        (either, other) match {
          case (Left(a), Left(b)) => Left(L.combine(a, b))
          case (Left(a), Right(_)) => Left(a)
          case (Right(_), Left(b)) => Left(b)
          case (Right(r), Right(r1)) => Right((r, r1))
        }

      /** Useful for `Checked` to combine both `Problem`s. */
      def combineLeftOrRight[R1](other: Either[L, R])(implicit L: Semigroup[L], R: Semigroup[R]): Either[L, R] =
        (either, other) match {
          case (Left(a), Left(b)) => Left(L.combine(a, b))
          case (Left(a), Right(_)) => Left(a)
          case (Right(_), Left(b)) => Left(b)
          case (Right(r), Right(r1)) => Right(R.combine(r, r1))
        }

      def orThrow: R =
        either match {
          case Left(problem: Problem) =>
            var t = problem.throwable
            if (t.getStackTrace.isEmpty) t = t.appendCurrentStackTrace
            throw t.dropTopMethodsFromStackTrace("orThrow$extension")

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
      def truncateWithEllipsis(n: Int, showLength: Boolean = false, firstLineOnly: Boolean = false)
      : String = {
        val suffix = if (showLength) s"$Ellipsis(length ${underlying.length})" else Ellipsis
        val nn = max(suffix.length, n)
        val firstLine = if (firstLineOnly) underlying.firstLineLengthN(nn) else underlying.length
        val result = underlying
          .pipeIf((nn min firstLine) < underlying.lengthIs)(_
            .take(nn - suffix.length min firstLineLength) + suffix)

        if (result.exists(_.isControl))
          result.map(c => if (c.isControl) '·' else c)
        else
          result
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

      def firstLineLength = firstLineLengthN(Int.MaxValue)

      def firstLineLengthN(until: Int): Int = {
        val n = until min underlying.length
        var i = 0
        while (i < n) {
          if(underlying.charAt(i) == '\n') {
            return if (i > 0 && underlying.charAt(i - 1) == '\r') i - 1 else i
          }
          i += 1
        }
        n
      }
    }

    implicit final class RichStringuilder(private val sb: StringBuilder) extends AnyVal
    {
      /** Right-adjust (moves) the text written by body and fill the left-side up with spaces. */
      def fillLeft(width: Int)(body: => Unit): Unit = {
        val insert = sb.length()
        body
        val shift = insert + width - sb.length()
        if (shift > 0) {
          if (shift <= spaceArray.length) {
            sb.insertAll(insert, spaceArray, 0, shift)
          } else {
            sb.insertAll(insert, Iterator.fill(shift)(' '))
          }
        }
      }

      /** Fill the right-side up with spaces after something has been written by body. */
      def fillRight(width: Int)(body: => Unit): Unit = {
        val right = sb.length() + width
        body
        val fill = right - sb.length()
        if (fill > 0) {
          if (fill <= spaceArray.length) {
            sb.appendAll(spaceArray, 0, fill)
          } else {
            sb.appendAll(Iterator.fill(fill)(' '))
          }
        }
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

  def withStringBuilder(body: StringBuilder => Unit): String =
    withStringBuilder(16)(body)

  def withStringBuilder(size: Int = 16)(body: StringBuilder => Unit): String = {
    val sb = new StringBuilder(size)
    body(sb)
    sb.toString
  }

  def chunkStrings(strings: Seq[String], maxSize: Int): Iterable[String] = {
    val total = strings.view.map(_.length).sum
    if (total == 0)
      Nil
    else if (total <= maxSize)
      strings.combineAll :: Nil
    else {
      val result = mutable.Buffer.empty[String]
      val sb = new StringBuilder(maxSize)
      for (str <- strings) {
        if (sb.isEmpty && str.length == maxSize) {
          result.append(str)
        } else {
          var start = 0
          while (start < str.length) {
            val end = (start + maxSize - sb.length) min str.length
            val a = str.substring(start, end)
            if (sb.isEmpty && a.length == maxSize) {
              result.append(a)
            } else {
              sb.append(a)
              if (sb.length == maxSize) {
                result.append(sb.toString)
                sb.clear()
              }
            }
            start += a.length
          }
        }
      }
      if (sb.nonEmpty) result.append(sb.toString)
      result
    }
  }
}
