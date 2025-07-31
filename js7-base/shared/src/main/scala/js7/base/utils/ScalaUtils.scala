package js7.base.utils

import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.{Foldable, Functor, Monad, Monoid, Semigroup}
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag
import java.io.{ByteArrayInputStream, InputStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.ReentrantLock
import java.util.{Formatter, Locale}
import js7.base.exceptions.PublicException
import js7.base.log.Logger
import js7.base.metering.CallMeter
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.utils.Ascii.toPrintableChar
import js7.base.utils.BinarySearch.binarySearch
import js7.base.utils.Nulls.nullToNone
import js7.base.utils.ScalaUtils.syntax.{RichString, RichThrowable}
import js7.base.utils.StackTraces.StackTraceThrowable
import scala.annotation.tailrec
import scala.collection.{AbstractIterator, AbstractMapView, Factory, MapOps, MapView, View, mutable}
import scala.math.Ordering.Implicits.*
import scala.math.max
import scala.reflect.ClassTag
import scala.util.boundary.break
import scala.util.chaining.*
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try, boundary}

object ScalaUtils:

  val RightUnit: Either[Nothing, Unit] = Right(())
  private val spaceArray = (" " * 64).toCharArray
  private val formatLocale = Locale.getDefault(Locale.Category.FORMAT)
  private lazy val makeUniqueMeter = CallMeter("makeUnique")
  private lazy val logger = Logger[this.type]
  private var simpleScalaClassNameCache = Map.empty[Class[?], String]

  //given iterableMonoid: Foldable[Iterable] =
  //  new Foldable[Iterable]:
  //    def foldLeft[A, B](fa: Iterable[A], b: B)(f: (B, A) => B): B =
  //      fa.foldLeft(b)(f)
  //
  //    def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
  //      ###

  object syntax:
    extension [A <: AnyRef](a: A)
      /** Like isInstanceOf, but compiles only if A1 is an A. */
      def isSubtypeOf[A1 <: A: ClassTag as A1]: Boolean =
        // TODO Could be an inline macro generating instanceOf
        a.isSubtypeOf(A1.runtimeClass)

      inline def isSubtypeOf[A1 <: A](cls: Class[A1]): Boolean =
        cls.isAssignableFrom(a.getClass)

    implicit final class RichF_[F[_], A](private val underlying: F[A]) extends AnyVal:
      def unless(condition: Boolean)(implicit F: Monoid[F[A]]): F[A] =
        when(!condition)

      def when(condition: Boolean)(implicit F: Monoid[F[A]]): F[A] =
        if condition then
          underlying
        else
          F.empty

    implicit final class RichEitherF[F[_], L, R](private val underlying: F[Either[L, R]]) extends AnyVal:
      def rightAs[R1](newRight: => R1)(implicit F: Functor[F]): F[Either[L, R1]] =
        F.map(underlying)(_.map(_ => newRight))

      def rightAs(unit: Unit)(implicit F: Functor[F]): F[Either[L, Unit]] =
        F.map(underlying):
          case Right(_) => RightUnit
          case o => o.asInstanceOf[Either[L, Unit]]

      /** mapmap(f) is equivalent to map(_ map f). */
      def mapmap[R1](f: R => R1)(implicit F: Functor[F]): F[Either[L, R1]] =
        F.map(underlying)(_.map(f))

      def mapT[L1 >: L, R1](f: R => Either[L1, R1])(implicit F: Functor[F]): F[Either[L1, R1]] =
        F.map(underlying):
          case Left(o) => Left(o)
          case Right(o) => f(o)

      /** Simple alternative to `EitherT` `flatMap` if for-comprehension is not needed. */
      def flatMapT[R1](f: R => F[Either[L, R1]])(implicit F: Monad[F]): F[Either[L, R1]] =
        F.flatMap(underlying):
          case Left(left) => F.pure(Left(left))
          case Right(right) => f(right)

      /** Like Applicative recover but for F[Either[L, R]]. */
      def recoverT[R1 >: R](f: PartialFunction[L, R1])(using F: Monad[F]): F[Either[L, R1]] =
        F.flatMap(underlying):
          case Left(left) if f isDefinedAt left => F.pure(Right(f(left)))
          case either => F.pure(either)

      /** Like Applicative recoverWith but for F[Either[L, R]]. */
      def recoverWithT[R1 >: R](f: PartialFunction[L, F[Either[L, R1]]])(using F: Monad[F])
      : F[Either[L, R1]] =
        F.flatMap(underlying):
          case Left(left) if f isDefinedAt left => f(left)
          case either => F.pure(either)


      // TODO A better name ?
      /** Simple alternative to `EitherT` `flatMap` if for-comprehension is not needed. */
      def flatMapRight[R1](f: R => F[Either[L, R1]])(implicit F: Monad[F]): F[Either[L, R1]] =
        flatMapT(f)

      /** Simple alternative to `EitherT` `flatMap` if for-comprehension is not needed. */
      def flatTapT[R1](f: R => F[Either[L, R1]])(implicit F: Monad[F]): F[Either[L, R]] =
        F.flatMap(underlying):
          case Left(left) => F.pure(Left(left))
          case Right(right) => f(right).map(_.as(right))

      def flatMapLeft[L1](f: L => F[Either[L1, R]])(implicit F: Monad[F]): F[Either[L1, R]] =
        F.flatMap(underlying):
          case Left(left) => f(left)
          case Right(right) => F.pure(Right(right))

      def flatMapLeftCase[R1 >: R](f: PartialFunction[L, F[Either[L, R1]]])(implicit F: Monad[F]): F[Either[L, R1]] =
        F.flatMap(underlying):
          case Left(left) => f.applyOrElse(left, (left: L) => F.pure(Left(left)))
          case Right(right) => F.pure(Right(right))

    // Does not work because rightAs() accepts any type as Unit. But we want accept Unit only.
    //implicit final class RichUnitEitherF[F[_], L](private val underlying: F[Either[L, Unit]])
    //extends AnyVal {
    //  @deprecated("❌Right is already Unit", "")
    //  def rightAs(unit: Unit)(implicit F: Functor[F]): F[Either[L, Unit]] =
    //    underlying
    //}

    implicit final class RichEitherIterable[F[x] <: Iterable[x], L, R](private val iterable: F[Either[L, R]])
    extends AnyVal:
      /** Combines left sides of any, otherwise return right sides.*/
      def reduceLeftEither(implicit F: Factory[R, F[R]], L: Semigroup[L]): Either[L, F[R]] =
        L.combineAllOption(iterable.view.collect { case Left(l) => l })
          .match
            case Some(problem) => Left(problem)
            case None => Right(iterable.view.collect { case Right(r) => r }.to(F))

    implicit final class RichLeftProjection[L, R](private val leftProjection: Either.LeftProjection[L, R])
    extends AnyVal:
      @throws[NoSuchElementException]
      def orThrow: L =
        leftProjection.e match
          case Left(a) => a
          case _       => throw new NoSuchElementException(s"Either.left.get on: ${leftProjection.e}")

    implicit final class RichOption[A](private val option: Option[A]) extends AnyVal:
      /** Like Scala's `fold`, but with proper result type derivation. */
      def fold_[B](ifNone: => B, ifSome: A => B): B =
        option match
          case None => ifNone
          case Some(a) => ifSome(a)

      def !!(problem: => Problem): Checked[A] =
        option match
          case None => Left(problem)
          case Some(a) => Right(a)

      /** Like Cats map2, but returns None only if both arguments are None. */
      def merge[B >: A](maybeB: Option[B])(f: (A, B) => B): Option[B] =
        option match
          case None => maybeB
          case aa @ Some(a) => maybeB match
            case None => aa
            case Some(b) => Some(f(a, b))


    extension[A](try_ : Try[A])
      /** Does not create an InvalidOperationException if Success. */
      def ifFailed: Option[Throwable] =
        try_ match
          case Failure(t) => Some(t)
          case Success(_) => None

    /** orElse inside a F[Option]. */
    implicit final class RichOptionF[F[_], A](private val underlying: F[Option[A]]) extends AnyVal:
      def orElseT(alternative: => F[Option[A]])(implicit F: Monad[F]): F[Option[A]] =
        F.flatMap(underlying):
          case None => alternative
          case Some(a) => F.pure(a.some)

      /** Simple alternative to `EitherT` `flatMap` if for-comprehension is not needed. */
      def flatMapT[A1](f: A => F[Option[A1]])(implicit F: Monad[F]): F[Option[A1]] =
        F.flatMap(underlying):
          case None => F.pure(None)
          case Some(a) => f(a)

    implicit final class RichJavaClass[A](private val underlying: Class[A]):
      def scalaName: String = underlying.getName stripSuffix "$"

      /** Simple name without companion object's '$'.
        * <p>
        * Calculated values are cached. */
      def simpleScalaName: String = simpleName stripSuffix "$"
        simpleScalaClassNameCache.getOrElse(underlying, (_: Class[?]) =>
          val name = underlying.getName stripSuffix "$"
          simpleScalaClassNameCache = simpleScalaClassNameCache.updated(underlying, name)
          name)

      /** Like getSimpleName, but even simpler. */
      def simpleName: String =
        simpleClassName(underlying.getName)

      /** Simple class name prefixed by maybe outer class name. */
      def shortClassName: String =
        if underlying.isPrimitive then
          underlying.getName.capitalize
        else
          // Change '$' inner class concatenation character to '.'
          val simpleName = simpleScalaName
          val prefix = scalaName stripSuffix simpleName
          val name = (if prefix.endsWith("$") then prefix.init + '.' else prefix) + simpleName
          removePackageRegex.replaceFirstIn(name, "")

    implicit final class ToStringFunction1[A, R](private val delegate: A => R):
      def withToString(string: String): A => R =
        new (A => R):
          def apply(a: A) = delegate(a)
          override def toString() = string

    implicit final class RichThrowable[A <: Throwable](private val throwable: A) extends AnyVal:
      def rootCause: Throwable =
        @tailrec def cause(t: Throwable): Throwable =
          t.getCause match
            case null => t
            case o if o == t => t
            case o => cause(o)
        cause(throwable)

      def toStringWithCausesAndStackTrace: String =
        throwable.toStringWithCauses +
          (throwable.getStackTrace.nonEmpty ?? ("\n" + throwable.stackTraceAsString))

      def toStringWithCauses: String =
        var result = throwable.toSimplifiedString
        throwable.getCause match
          case null =>
          case cause =>
            val c = cause.toStringWithCauses
            if !result.contains(c) then
              result = result.stripSuffix(":") + ", caused by: " + c
        if throwable.getSuppressed.nonEmpty then
          result += throwable.getSuppressed
            .map(t => " [suppressed: " + t.toStringWithCauses + "]")
            .mkString
        result

      def toSimplifiedString: String =
        lazy val msg = throwable.getMessage
        throwable.match
          case _: java.lang.IllegalStateException
               | _: java.lang.NumberFormatException
               | _: java.lang.IndexOutOfBoundsException
               | _: java.lang.StringIndexOutOfBoundsException
               | _: java.lang.ArrayIndexOutOfBoundsException =>
            throwable.toString.stripPrefix("java.lang.")

          case _: java.util.NoSuchElementException =>
            throwable.toString.stripPrefix("java.util.")

          case _: ArithmeticException if msg != null =>
            s"ArithmeticException: $msg"

          case _: java.util.concurrent.TimeoutException =>
            throwable.toString.stripPrefix("java.util.concurrent.")

          case _: java.nio.file.NoSuchFileException =>
            throwable.toString.stripPrefix("java.nio.file.")

          case _ =>
            if msg != null && msg != "" && (
              throwable.isInstanceOf[ProblemException] ||
                throwable.getClass == classOf[IllegalArgumentException] ||
                throwable.getClass == classOf[RuntimeException] ||
                throwable.getClass == classOf[Exception] ||
                throwable.isInstanceOf[PublicException] ||
                throwable.getClass.getName == "scala.scalajs.js.JavaScriptException" ||
                throwable.getClass.getName == "java.util.concurrent.CompletionException")
            then
              msg
            else
              throwable.toString
        .trim

      def stackTraceAsString: String =
        val w = new StringWriter
        throwable.printStackTrace(new PrintWriter(w))
        w.toString

      /** Useable for logging.
        * `logger.info(throwable.toStringWithCauses, throwable.nullIfNoStackTrace)` */
      def nullIfNoStackTrace: Throwable | Null =
        if throwable.getStackTrace.isEmpty then null else throwable

      def ifStackTrace: Option[Throwable] =
        nullToNone(nullIfNoStackTrace)

      def dropTopMethodsFromStackTrace(methodName: String): A =
        val stackTrace = throwable.getStackTrace
        var i = 0
        while i < stackTrace.length && stackTrace(i).getMethodName == methodName do i += 1
        if i > 0 then throwable.setStackTrace(stackTrace.drop(i))
        throwable

    implicit final class RichAny[A](private val delegate: A) extends AnyVal:
      /** Apply the function. */
      inline def |>[B](f: A => B): B =
        delegate.pipe(f)

      /** Apply the function conditionally. */
      def pipeIf[B >: A](condition: => Boolean)(f: A => B): B =
        if condition then f(delegate) else delegate

      /** Apply the function conditionally. */
      def pipeMaybe[O, B >: A](maybe: => Option[O])(f: (A, O) => B): B =
        maybe match
          case Some(o) => f(delegate, o)
          case None => delegate

      def narrow[B <: A: ClassTag]: Checked[B] =
        checkedCast[B](delegate)

      @inline def substitute(when: A, _then: => A): A =
        if delegate == when then _then else delegate

    implicit final class SwitchOnAtomicBoolean(private val delegate: AtomicBoolean) extends AnyVal:
      def switchOn(body: => Unit): Unit =
        if delegate.compareAndSet(false, true) then body

      def switchOff(body: => Unit): Unit =
        if delegate.compareAndSet(true, false) then body

    implicit final class RichView[A](private val view: View[A]) extends AnyVal:
      inline def :+[B >: A](b: B): View[B] =
        appended(b)

      def appended[B >: A](b: B): View[B] =
        view.concat(b :: Nil)

      def +:[B >: A](b: B): View[B] =
        new View.Single(b) ++ view


    extension [A](iterable: Iterable[A])
      def foldMap[B: Monoid as B](f: A => B): B =
        //iterable.foldLeft(B.empty)((b, a) => B.combine(b, f(a)))
        iterable match
          case seq: Seq[A] => Foldable[Seq].foldMap(seq)(f)
          case _ => B.combineAll(iterable.map(f))

      /** Like mkString but limits the number of shown elements.
        *
        * With a dynamically calculated Iterable of unknown size,
        * the first n elements may be calculated twice.
        */
      def mkStringLimited(n: Int, separator: String = ", "): String =
        val size = iterable.size
        if size <= n then
          iterable.mkString(separator)
        else
          s"${iterable.take(n).mkString("", separator, separator)}and ${size - n} more"

    extension [CC[a] <: Iterable[a], A](iterable: CC[A])(using C: Factory[A, CC[A]])
      def takeUntil(predicate: A => Boolean): CC[A] =
        takeThrough(a => !predicate(a))

      def takeThrough(predicate: A => Boolean): CC[A] =
        val builder = C.newBuilder
        val iterator = iterable.iterator
        var continue = true
        while continue && iterator.hasNext do
          val a = iterator.next()
          builder += a
          continue = predicate(a)
        builder.result()

    extension [A](iterator: Iterator[A])
      def takeUntil(predicate: A => Boolean): Iterator[A] =
        takeThrough(a => !predicate(a))

      def takeThrough(predicate: A => Boolean): Iterator[A] =
        var firstNonMatching: Iterator[A] = Iterator.empty
        iterator.takeWhile: a =>
          val p = predicate(a)
          if !p then firstNonMatching = Iterator.single(a)
          p
        .concat:
          firstNonMatching

      def continueWithLast: Iterator[A] =
        val isNonEmpty = iterator.nonEmpty
        val iterator_ = iterator
        case object Empty
        new AbstractIterator[A]:
          var last: A | Empty.type = Empty
          def hasNext = isNonEmpty

          def next() =
            if iterator_.hasNext then
              val a = iterator_.next()
              last = a
              a
            else
              last match
                case Empty => throw new NoSuchElementException
                case a: A @unchecked => a

    extension [A](iterableOnce: IterableOnce[A])
      /** Convert to Seq[A].
       * <p>The original toSeq method is deprecated..
       * <p>Use this method only once! */
      def asSeqOrToVector: Seq[A] =
        iterableOnce match
          case o: Seq[A] => o
          case _ => Vector.from(iterableOnce)

      /** Make an eagerly computed Seq.
       * <p>The original toSeq method is deprecated..
       * <p>Use this method only once! */
      def toEagerSeq: Seq[A] =
        iterableOnce match
          case o: scala.collection.immutable.List[A] => o
          case o: scala.collection.immutable.ArraySeq[A] => o
          case o: scala.collection.immutable.Queue[A] => o
          case o: fs2.Chunk[A @unchecked] => o.asSeq
          case _ => Vector.from(iterableOnce)

      def foldMap[B: Monoid as B](f: A => B): B =
        iterableOnce match
          case seq: Seq[A] => Foldable[Seq].foldMap(seq)(f)
          case iterable: Iterable[A] => iterable.foldMap(f)
          case _ => iterableOnce.iterator.foldLeft(B.empty)((b, a) => B.combine(b, f(a)))
        //iterableOnce match
        //  case iterable: Iterable[A] => B.combineAll(iterable.map(f))
        //  case _ => B.combineAll(iterableOnce.iterator.map(f))

      def repeatLast: LazyList[A] =
        iterableOnce match
          case seq: IndexedSeq[A] => seq ++: LazyList.continually(seq.last)
          case _ => LazyList.from(iterableOnce.iterator.continueWithLast)

      //inline def foldChecked[S](init: S)(f: (S, A) => Checked[S]): Checked[S] =
      //  foldEithers[S, Problem](init)(f)

      def foldEithers[S, L](init: S)(f: (S, A) => Either[L, S]): Either[L, S] =
        var left: L | Null = null
        var s = init
        val it = iterableOnce.iterator

        while it.hasNext && left == null do
          val a = it.next()
          f(s, a) match
            case Left(lft) => left = lft
            case Right(o) => s = o

        if left != null then Left(left) else Right(s)


      /** Usable for logging a chunk of lines with a long bracket. */
      def foreachWithBracket(bracket: MultipleLinesBracket = MultipleLinesBracket.Square)
        (body: (A, Char) => Unit)
      : Unit =
        val iterator = iterableOnce.iterator
        if iterator.hasNext then
          var a = iterator.next()
          if !iterator.hasNext then
            body(a, bracket.single)
          else
            body(a, bracket.first)
            a = iterator.next()
            while iterator.hasNext do
              body(a, bracket.middle)
              a = iterator.next()
            body(a, bracket.last)


    implicit final class RichIterables[A](private val iterables: IterableOnce[IterableOnce[A]])
    extends AnyVal:

      def mergeOrdered(implicit A: Ordering[A]): Iterator[A] =
        mergeOrderedBy(identity)

      def mergeOrderedBy[B: Ordering](f: A => B): Iterator[A] =
        mergeOrderedOptimizedBy(f)

      def mergeOrderedSlowBy[B: Ordering](f: A => B): Iterator[A] =
        val bufferedIterators = iterables.iterator.map(_.iterator.buffered).toVector
        Iterator.unfold(())(_ =>
          bufferedIterators
            .filter(_.hasNext)
            .map(it => it -> it.head)
            .minByOption(o => f(o._2))
            .map { case (iterator, a) =>
              iterator.next()
              a -> ()
            })

      // About twice as fast
      def mergeOrderedOptimizedBy[B: Ordering](f: A => B): scala.collection.BufferedIterator[A] =
        new MergeOrderedIterator(iterables, f)


    //extension methods collide with other names in the namespace, for example pekko-http get
    //extension [A](seq: collection.Seq[A])
    implicit final class RichSeq[A](private val seq: Seq[A]) extends AnyVal:
      def get(i: Int): Option[A] =
        try
          seq(i).some
        catch case _: IndexOutOfBoundsException =>
          None

      def checked(i: Int)(using filename: sourcecode.FileName, line: sourcecode.Line): Checked[A] =
        try
          Right(seq(i))
        catch case t: IndexOutOfBoundsException =>
          Left:
            val range = seq.knownSize match
              case -1 => ""
              case n => s" 0...${n - 1}"
            Problem(s"Index $i is out of bounds$range in ${filename.value}:${line.value}")

    extension [A](vector: Vector[A])
      /** Insert into an ordered sequence. */
      def insertOrdered(a: A)(using Ordering[A]): Vector[A] =
        val i = binarySearch(vector)(a)._1
        vector.take(i) :+ a :++ vector.drop(i)


    implicit final class RichScalaUtilsMap[K, V](private val underlying: MapOps[K, V, ?, ?])
    extends AnyVal:
      def checked(key: K)(using K: Tag[K], x: sourcecode.FileName, y: sourcecode.Line): Checked[V] =
        rightOr(key, UnknownKeyProblem(K.tag.shortName, key))

      def rightOr(key: K, notFound: => Problem): Checked[V] =
        underlying.get(key) match
          case None => Left(notFound)
          case Some(a) => Right(a)

    implicit final class RichMapView[K, V](private val mapView: MapView[K, V])
    extends AnyVal:
      def collectValues[V1](pf: PartialFunction[V, V1]): MapView[K, V1] =
        mapView
          .mapValues(pf.lift)
          .filter(_._2.isDefined)
          .mapValues(_.get)

      /** concat reversely, left side overrides right side. */
      def orElseMapView[V1 >: V](other: MapView[K, V1]): MapView[K, V1] =
        new MapView[K, V1]:
          def get(key: K) =
            mapView.get(key) orElse other.get(key)

          def iterator =
            mapView.iterator ++ other.filterKeys(k => !mapView.contains(k))

          override def keySet =
            mapView.keySet ++ other.keySet

          override def isEmpty =
            mapView.isEmpty && other.isEmpty

      /** K1 and K must be isomorphic, fromK1(toK1(k)) == k), maybe keeping the ordering.
       * @param fromK1 A NonFatal exception will be ignored
       */
      def mapIsomorphic[K1, V1](toK1: K => K1, toV1: V => V1)(fromK1: K1 => K): MapView[K1, V1] =
        new MapView[K1, V1]:
          def get(k1: K1) =
            tryFromK1(k1)
              .flatMap(mapView.get)
              .map(toV1)

          override def contains(k1: K1) =
            tryFromK1(k1).exists(mapView.contains)

          private def tryFromK1(k1: K1) =
            Try(fromK1(k1)).toOption

          def iterator =
            mapView.iterator.map: (k, v) =>
              toK1(k) -> toV1(v)

          override def keySet =
            mapView.keySet.map(toK1)

          override def keysIterator =
            mapView.keysIterator.map(toK1)

          override def values =
            mapView.values.view.map(toV1)

          override def isEmpty =
            mapView.isEmpty

          override def knownSize =
            mapView.knownSize

      /** Concat to MapViews and return a MapView wit Map behaviour. */
      def +++[V1 >: V](right: MapView[K, V1]): MapView[K, V1] =
        new AbstractMapView[K, V1]:
          def get(key: K): Option[V1] =
            right.get(key) match
              case o @ Some(_) => o
              case _ => mapView.get(key)

          def iterator: Iterator[(K, V1)] =
            mapView.iterator
              .filter { case (k, _) => !right.contains(k) }
              .concat(right.iterator)

    private[utils] final class MergeOrderedIterator[A, B: Ordering] private[ScalaUtils](
      iterables: IterableOnce[IterableOnce[A]],
      f: A => B)
    extends collection.BufferedIterator[A]:
      private var minimum = none[A]
      private val bufferedIterators =
        iterables.iterator.map(_.iterator.buffered).to(mutable.ArrayBuffer)

      override def buffered: MergeOrderedIterator.this.type = this

      def head: A =
        if minimum.isEmpty then compute()
        minimum.getOrElse(throw new NoSuchElementException)

      override def headOption: Option[A] =
        minimum match
          case None => hasNext thenMaybe minimum
          case o => o

      def hasNext: Boolean =
        minimum.isDefined || {
          compute()
          minimum.isDefined
        }

      def next(): A =
        val result = head
        minimum = None
        result

      private def compute(): Unit =
        minimum = None
        var selectedIterator: Iterator[A] = null.asInstanceOf[Iterator[A]]
        var i = 0
        while i < bufferedIterators.length do
          val iterator = bufferedIterators(i)
          if !iterator.hasNext then
            bufferedIterators.remove(i)
          else
            val a = iterator.head
            if minimum.forall(x => f(x) > f(a)) then
              minimum = Some(a)
              selectedIterator = iterator
            i += 1
        if minimum.isDefined then selectedIterator.next()

    // Like PartialFunction.Lifted:
    private val fallback_fn: Any => Any = _ => fallback_fn
    private def checkFallback[B] = fallback_fn.asInstanceOf[Any => B]
    private def fallbackOccurred[B](x: B) = fallback_fn eq x.asInstanceOf[AnyRef]

    implicit final class RichPartialFunction[A, B](private val underlying: PartialFunction[A, B])
    extends AnyVal:
      def get(key: A): Option[B] =
        val b = underlying.applyOrElse(key, checkFallback[B])
        if fallbackOccurred(b) then None else Some(b)

      def checked(key: A)(using A: Tag[A]): Checked[B] =
        val b = underlying.applyOrElse(key, checkFallback[B])
        if fallbackOccurred(b) then
          Left(UnknownKeyProblem(A.tag.shortName, key))
        else
          Right(b)

      def rightOr(key: A, notFound: => Problem): Checked[B] =
        toChecked(_ => notFound)(key)

      private def toChecked(notFound: A => Problem): A => Checked[B] =
        key =>
          val b = underlying.applyOrElse(key, checkFallback[B])
          if fallbackOccurred(b) then
            Left(notFound(key))
          else
            Right(b)

      def checkNoDuplicate(key: A)(implicit A: ClassTag[A]): Checked[Unit] =
        if underlying isDefinedAt key then
          Left(DuplicateKey(A.runtimeClass.shortClassName, key))
        else
          RightUnit

      def getOrElse[BB >: B](key: A, default: => BB): BB =
        underlying.applyOrElse(key, (_: A) => default)

      def map[C](f: B => C): PartialFunction[A, C] =
        mapPartialFunction(f)

      def mapPartialFunction[C](bToC: B => C): PartialFunction[A, C] =
        new PartialFunction[A, C]:
          def isDefinedAt(a: A) = underlying.isDefinedAt(a)

          def apply(a: A) = bToC(underlying.apply(a))

          override def applyOrElse[A1 <: A, C1 >: C](a: A1, default: A1 => C1): C1 =
            val b = underlying.applyOrElse(a, checkFallback[B])
            if fallbackOccurred(b) then
              default(a)
            else
              bToC(b)

    private val SomeTrue = Some(true)

    implicit final class RichBoolean(private val underlying: Boolean) extends AnyVal:
      /**
        * Conditional `Option`.
        * <p>`(true ? a) == Some(a)`
        * <br>`(false ? a) == None`
        */
      inline def ?[A](a: => A): Option[A] =
        thenSome(a)

      ///**
      //  * Conditional `Option`.
      //  * <p>`(1 == 1 &? a) == Some(a)`
      //  * <br>`(1 != 1 &? a) == None`
      //  */
      //def &?[A](a: => A): Option[A] =
      //  thenSome(a)

      /** Optional false.
        * <p>`true.? == Some(a)`
        * <br>`false.? == None`
        */
      def ? : Option[Boolean] =
        if underlying then SomeTrue else None

      /**
        * Conditional `Option`.
        * <p>`true.thenSome(a) == Some(a)`
        * <br>`false.thenSome(a) == None`
        */
      infix def thenSome[A](a: => A): Option[A] =
        if underlying then Some(a) else None

      /**
        * Conditional `Option`.
        * <p>`true.thenMaybe(Some(7)) == Some(7)`
        * <p>`true.thenMaybe(None) == None`
        * <br>`false.thenMaybe(Some(7)) == None`
        * <br>`false.thenMaybe(None) == None`
        */
      infix def thenMaybe[A](maybe: => Option[A]): Option[A] =
        if underlying then maybe else None

      /**
        * Conditional `List`.
        * <p>`(true option a) == List(a)`
        * <br>`(false option a) == Nil`
        */
      infix def thenList[A](a: => A): List[A] =
        if underlying then a :: Nil else Nil

      /**
        * Conditional `Vector`.
        * <p>`(true option a) == Vector(a)`
        * <br>`(false option a) == Vector.empty`
        */
      infix def thenVector[A](a: => A): Vector[A] =
        if underlying then Vector(a) else Vector.empty

      /**
        * Conditional `Set`.
        * <p>`(true option a) == Set(a)`
        * <br>`(false option a) == Set.empty`
        */
      infix def thenSet[A](a: => A): Set[A] =
        if underlying then Set(a) else Set.empty

      /**
        * Conditional `Iterator`.
        * <p>`(true option a) == Iterator(a)`
        * <br>`(false option a) == Iterator.empty`
        */
      infix def thenIterator[A](a: => A): Iterator[A] =
        if underlying then Iterator.single(a) else Iterator.empty

      /**
        * Conditional `View`.
        * <p>`(true option a) == View.Single(a)`
        * <br>`(false option a) == View.empty`
        */
      infix def thenView[A](a: => A): View[A] =
        if underlying then new View.Single(a) else View.empty

      /**
        * Conditional `fs2.Stream`.
        * <p>`(true option a) == Stream.emit(a)`
        * <br>`(false option a) == Stream.emit`
        */
      infix def thenStream[A](a: => A): fs2.Stream[fs2.Pure, A] =
        if underlying then fs2.Stream.emit(a) else fs2.Stream.empty
      /** The string on the right side if true, otherwise the empty string. */
      def ??(string: => String): String =
        if underlying then string else ""

      inline def !![L](left: => L): Either[L, Unit] =
        orLeft(left)

      def orLeft[L](left: => L): Either[L, Unit] =
        if !underlying then Left(left)
        else RightUnit

      def toInt: Int =
        if underlying then 1 else 0

    implicit final class RichEither[L, R](val either: Either[L, R]) extends AnyVal:
      def rightAs[R1](newRight: => R1): Either[L, R1] =
        either.map(_ => newRight)

      def rightAs(right: Unit): Either[L, Unit] =
        either match
          case Right(_) => RightUnit
          case _ => either.asInstanceOf[Either[L, Unit]]

      //def flatMapLeft[L1](f: L => Either[L1, R]): Either[L1, R] =
      //  either match {
      //    case Left(o) => f(o)
      //    case o => o.asInstanceOf[Either[L1, R]]
      //  }

      /** Useful for `Checked` to combine both `Problem`s. */
      def combineLeft[R1](other: Either[L, R1])(implicit L: Semigroup[L]): Either[L, (R, R1)] =
        (either, other) match
          case (Left(a), Left(b)) => Left(L.combine(a, b))
          case (Left(a), Right(_)) => Left(a)
          case (Right(_), Left(b)) => Left(b)
          case (Right(r), Right(r1)) => Right((r, r1))

      /** Useful for `Checked` to combine both `Problem`s. */
      def combineLeftOrRight[R1](other: Either[L, R])(implicit L: Semigroup[L], R: Semigroup[R]): Either[L, R] =
        (either, other) match
          case (Left(a), Left(b)) => Left(L.combine(a, b))
          case (Left(a), Right(_)) => Left(a)
          case (Right(_), Left(b)) => Left(b)
          case (Right(r), Right(r1)) => Right(R.combine(r, r1))

      def orThrow: R =
        either match
          case Left(problem: Problem) =>
            var t = problem.throwable
            if t.getStackTrace.isEmpty then t = t.appendCurrentStackTrace
            throw t.dropTopMethodsFromStackTrace("orThrow$extension")

          case Left(_) =>
            throw new NoSuchElementException(s"Either.orThrow on $either")
              .dropTopMethodsFromStackTrace("orThrow$extension")

          case Right(r) => r

      def orThrowWithoutOurStackTrace: R =
        either match
          case Left(problem: Problem) =>
            logger.debug(s"💥 $problem", problem.throwable)
            throw problem.throwable.dropTopMethodsFromStackTrace("orThrow$extension")

          case Left(_) =>
            throw new NoSuchElementException(s"Either.orThrow on $either")
              .dropTopMethodsFromStackTrace("orThrow$extension")

          case Right(r) => r

      def leftOrThrow: L =
        either match
          case Left(l) => l
          case Right(_) => throw new NoSuchElementException(s"Either.orThrow on $either")
            .dropTopMethodsFromStackTrace("orThrow$extension")

      def tapEach(tapRight: R => Unit): either.type =
        either match
          case Left(_) =>
          case Right(right) => tapRight(right)
        either

      def tapLeft(tapLeft: L => Unit): either.type =
        either match
          case Left(left) => tapLeft(left)
          case Right(_) =>
        either

    implicit final class RichThrowableEither[L <: Throwable, R](private val underlying: Either[L, R]) extends AnyVal:
      def orThrow: R =
        orThrow(_.dropTopMethodsFromStackTrace("orThrow$extension"))

      def orThrow(toThrowable: L => Throwable): R =
        underlying match
          case Left(t) => throw toThrowable(t.appendCurrentStackTrace)
          case Right(o) => o

      /** Converts an `Either[Throwable, A]` to a Checked[A] with complete Throwable. */
      def toThrowableChecked: Checked[R] =
        underlying match
          case Left(t) => Left(Problem.fromThrowable(t.appendCurrentStackTrace))
          case Right(o) => Right(o)

      /** Converts an `Either[Throwable, A]` to a Checked[A] with the `Throwable`'s message only (toStringWithCauses). */
      def toMessageOnlyChecked: Checked[R] =
        underlying match
          case Left(t) => Left(Problem.pure(Option(t.getMessage) getOrElse t.toStringWithCauses))
          case Right(o) => Right(o)

      def withStackTrace: Either[Throwable, R] =
        underlying match
          case o: Right[L, R] =>
            o

          case Left(t) if t.getStackTrace.nonEmpty =>
            Left(t)

          case Left(t) =>
            t.fillInStackTrace()
            Left(if t.getStackTrace.nonEmpty then t else new IllegalStateException(s"$t", t))

    extension (char: Char)
      inline def utf8Length: Int =
        ScalaUtils.utf8Length(char)

    extension (string: String)
      def indexOfOrLength(char: Char): Int =
        string.indexOf(char) match
          case -1 => string.length
          case i => i

    implicit final class RichString(private val underlying: String) extends AnyVal:
      /** Counts bytes of UTF-16 to UTF-8 encoding, the result may be bigger. */
      def estimateUtf8Length: Int =
        var byteCount = 0
        val len = underlying.length
        var i = 0
        while i < len do
          byteCount += underlying.charAt(i).utf8Length
          i += 1
        byteCount

      //Java 9:
      //def utf8Length: Int =
      //  var byteCount = 0
      //  val iterator = underlying.codePoints.iterator
      //  while iterator.hasNext do
      //    byteCount += ScalaUtils.utf8Length(iterator.next())
      //  byteCount

      /** Truncate to `n`, replacing the tail with ellipsis and, if the string is long, the total character count. */
      def truncateWithEllipsis(
        n: Int,
        showLength: Boolean = false,
        firstLineOnly: Boolean = false,
        quote: Boolean = false)
      : String =
        if underlying.length <= n && !quote && !underlying.exists(_.isControl) then
          underlying
        else
          val sb = new StringBuilder(n + 2 * quote.toInt)
          if quote then sb.append('»')
          val suffix = if showLength then s"...(length ${underlying.length})" else "..."
          val nn = max(/*suffix.length*/3 , n)
          val firstLine = if firstLineOnly then underlying.firstLineLengthN(nn) else underlying.length
          val truncate = (nn min firstLine) < underlying.length
          val truncateAt = if truncate then (nn - suffix.length) min firstLine else underlying.length

          var i = 0
          while i < truncateAt do
            sb.append(toPrintableChar(underlying(i)))
            i += 1

          if quote then sb.append('«')
          if truncate then sb.append(suffix)
          sb.toString

      def replaceChar(from: Char, to: Char): String =
        if underlying contains from then
          val chars = new Array[Char](underlying.length)
          underlying.getChars(0, underlying.length, chars, 0)
          for i <- chars.indices do if chars(i) == from then chars(i) = to
          new String(chars)
        else
          underlying

      def dropLastWhile(predicate: Char => Boolean): String =
        var i = underlying.length
        while i > 0 && predicate(underlying(i - 1)) do i = i -1
        underlying.substring(0, i)

      /** Length of the first line, including \n. */
      def firstLineLength: Int =
        firstLineLengthN(Int.MaxValue)

      def firstLineLengthN(until: Int): Int =
        val n = until min underlying.length
        var i = 0
        while i < n do
          if underlying.charAt(i) == '\n' then return i + 1
          i += 1
        n

    extension (problemOrNull: Problem | Null)
      def toChecked: Checked[Unit] =
        toLeftOr(())

      def toLeftOr[R](right: R): Checked[R] =
        problemOrNull match
          case null => Right(right)
          case o: Problem => Left(o)


    implicit final class RichStringBuilder(private val sb: StringBuilder) extends AnyVal:
      /** Right-adjust (moves) the text written by body and fill the left-side up with spaces. */
      def fillLeft(width: Int)(body: => Unit): Unit =
        val insert = sb.length()
        body
        val shift = insert + width - sb.length()
        if shift > 0 then
          if shift <= spaceArray.length then
            sb.insertAll(insert, spaceArray, 0, shift)
          else
            sb.insertAll(insert, Iterator.fill(shift)(' '))

      /** Fill the right-side up with spaces after something has been written by body. */
      def fillRight(width: Int)(body: => Unit): Unit =
        val right = sb.length() + width
        body
        val fill = right - sb.length()
        if fill > 0 then
          if fill <= spaceArray.length then
            sb.appendAll(spaceArray, 0, fill)
          else
            sb.appendAll(Iterator.fill(fill)(' '))

    implicit final class RichByteArray(private val underlying: Array[Byte]) extends AnyVal:
      def indexOfByte(byte: Byte): Int =
        val len = underlying.length
        var i = 0
        while i < len && underlying(i) != byte do i = i + 1
        if i == len then -1 else i
  end syntax

  extension (lock: ReentrantLock)
    def use[A](body: => A): A =
      lock.lock()
      try body
      finally lock.unlock()

  def reuseIfEqual[A <: AnyRef](a: A)(f: A => A): A =
    reuseIfEqual(a, f(a))

  def reuseIfEqual[A <: AnyRef](a: A, b: A): A =
    if a == b then a else b

  def implicitClass[A: ClassTag]: Class[A] =
    implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]

  def implicitTypeTag[A](implicit A: Tag[A]): LightTypeTag =
    A.tag

  def implicitTypeRepr[A](implicit A: Tag[A]): String =
    A.tag.repr.replace("[=", "[")

  private val removePackageRegex = """^([a-z0-9]*\.)*""".r

  private[utils] def simpleClassName[A](name: String): String =
    name.substring:
      1 + max(
        name.lastIndexOf('.', name.length - 2),
        name.lastIndexOf('$', name.length - 2))

  /**
    * Replaces toString of the body (argumentless function), for logging and debugging.
    */
  def function0WithToString[R](lazyString: => String)(body: => R): () => R =
    new (() => R):
      def apply() = body
      override def toString() = lazyString

  /**
   * Replaces toString of the function, for logging and debugging.
   */
  def function1WithToString[A, R](string: String)(function: A => R): A => R =
    new (A => R):
      def apply(a: A) = function(a)
      override def toString() = string

  def namedIdentity[A]: A => A =
    new (A => A):
      def apply(a: A) = a
      override def toString = "identity"

  def cast[A: ClassTag](o: Any): A =
    checkedCast[A](o) match
      case Left(problem) =>
        throw problem.throwableOption getOrElse new ClassCastException(problem.toString)
      case Right(a) => a

  def checkedCast[A: ClassTag](o: Any): Checked[A] =
    checkedCast[A](o,
      Problem(s"Expected ${implicitClass[A].getName} but got ${o.getClass.getName}: ${o.toString.truncateWithEllipsis(30)}"))

  def checkedCast[A: ClassTag](o: Any, problem: => Problem): Checked[A] =
    val A = implicitClass[A]
    if o == null then
      Left(Problem.fromThrowable(new NullPointerException(s"Expected ${A.getName}, found: null")))
    else if A.isAssignableFrom(o.getClass) then
      Right(o.asInstanceOf[A])
    else
      Left(problem)

  def ifCast[A: ClassTag](o: Any): Option[A] =
    if o == null then
      None
    else if implicitClass[A].isAssignableFrom(o.getClass) then
      Some(o.asInstanceOf[A])
    else
      None

  def some[A](a: A): Option[A] =
    Some(a)

  def someUnless[A](a: A, none: A): Option[A] =
    if a == none then None else Some(a)

  def orderingBy[A, B1, B2](toB1: A => B1, toB2: A => B2)
    (using B1: Ordering[B1], B2: Ordering[B2])
  : Ordering[A] =
    (a, b) =>
      B1.compare(toB1(a), toB1(b)) match
        case 0 => B2.compare(toB2(a), toB2(b))
        case o => o

  def orderingBy[A, B1, B2, B3](toB1: A => B1, toB2: A => B2, toB3: A => B3)
    (using B1: Ordering[B1], B2: Ordering[B2], B3: Ordering[B3])
  : Ordering[A] =
    (a, b) =>
      orderingBy(toB1, toB2).compare(a, b) match
        case 0 => B3.compare(toB3(a), toB3(b))
        case o => o

  /** Simple implementation (for tests), converts the string to an Array[Byte],
    * risking `OutOfMemoryError` for long Strings. */
  def shortStringToInputStream(string: String): InputStream =
    new ByteArrayInputStream(string.getBytes(UTF_8))  // OutOfMemoryError

  private val lowerCaseHex: Array[Char] = "0123456789abcdef".toArray

  def bytesToHex(bytes: collection.Seq[Byte]): String =
    val sb = new StringBuilder(2 * bytes.length)
    for b <- bytes.iterator do
      sb.append(lowerCaseHex((b >> 4) & 0xf))
      sb.append(lowerCaseHex(b & 0xf))
    sb.toString

  def withStringBuilder(body: StringBuilder => Unit): String =
    withStringBuilder(16)(body)

  def withStringBuilder(size: Int = 16)(body: StringBuilder => Unit): String =
    val sb = new StringBuilder(size)
    body(sb)
    sb.toString

  def chunkStrings(strings: Seq[String], maxSize: Int): Iterable[String] =
    val total = strings.view.map(_.length).sum
    if total == 0 then
      Nil
    else if total <= maxSize then
      strings.combineAll :: Nil
    else
      val result = mutable.Buffer.empty[String]
      val sb = new StringBuilder(maxSize)

      for str <- strings do
        if sb.isEmpty && str.length == maxSize then
          result.append(str)
        else
          var start = 0
          while start < str.length do
            val end = (start + maxSize - sb.length) min str.length
            val a = str.substring(start, end)
            if sb.isEmpty && a.length == maxSize then
              result.append(a)
            else
              sb.append(a)
              if sb.length == maxSize then
                result.append(sb.toString)
                sb.clear()
            start += a.length

      if sb.nonEmpty then result.append(sb.toString)
      result

  def ordinalToString(n: Int): String =
    n match
      case 1 => "1st"
      case 2 => "2nd"
      case 3 => "3rd"
      case n => s"${n}th"

  /** Only to let the compiler check the body, nothing is executed. */
  inline def compilable(inline body: Any): Unit = {}

  def utf8Length(char: Int): Int =
    val c = char & 0x7fffffff
    if c <= 0x7f then 1
    else if c <= 0x07ff then 2
    else if c <= 0xffff then 3
    else 4

  def parameterListToString(args: IterableOnce[Any]*): String =
    val iterator = args.iterator.flatten
    if iterator.isEmpty then
      ""
    else
      iterator.mkString("(", ", ", ")")

  /** @param pattern a `java.util.Formatter` pattern
    */
  def makeUnique(pattern: String, exists: String => Boolean): Checked[String] =
    makeUniqueMeter:
      if pattern.endsWith("%d") && pattern.lastIndexOf('%', pattern.length - 3) == -1 then
        // Optimized
        val prefix = pattern.dropRight(2)
        Right:
          findUnique(exists)(prefix + _)
      else
        try
          boundary:
            val sb = new java.lang.StringBuilder
            val formatter = new Formatter(sb, formatLocale)
            val result =
              findUnique(exists): i =>
                sb.setLength(0)
                formatter.format(pattern, i)
                val result = sb.toString
                if result == pattern then
                  break(Left(Problem("Invalid pattern for makeUnique function")))
                result
            Right(result)
        catch case NonFatal(e) =>
          Left(Problem(s"makeUnique function: ${e.toStringWithCauses}"))

  private def findUnique(exists: String => Boolean)
    (make: Int => String)
  : String =
    var i = 1
    while true do
      val s = make(i)
      if !exists(s) then return s
      i += 1
    null.asInstanceOf[String]

  /** Like a `let a <- expr in body(a)`. */
  final inline def eval[A, B](inline expr: A)(inline body: A => B): B =
    body(expr)
