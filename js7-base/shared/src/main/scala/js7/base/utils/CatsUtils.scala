package js7.base.utils

import cats.data.{NonEmptyList, NonEmptySeq, NonEmptyVector, Validated}
import cats.effect.{Fiber, FiberIO, IO, MonadCancel, Outcome, OutcomeIO, Resource, Sync, SyncIO}
import cats.kernel.Monoid
import cats.syntax.all.*
import cats.{Applicative, FlatMap, Traverse}
import izumi.reflect.Tag
import java.io.{ByteArrayInputStream, InputStream}
import java.util.Base64
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.catsutils.{CatsDeadline, UnsafeMemoizable}
import js7.base.log.{LogLevel, Logger}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.*
import scala.collection.View
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
object CatsUtils:

  type Nel[+A] = NonEmptyList[A]
  val Nel: NonEmptyList.type = NonEmptyList

  type Nev[+A] = NonEmptyVector[A]
  val Nev: NonEmptyVector.type = NonEmptyVector

  private val logger = Logger[this.type]

  def combine[A: Monoid](as: A*): A =
    as.combineAll

  private def bytesToInputStreamResource(bytes: collection.Seq[Byte]): Resource[SyncIO, InputStream] =
    bytesToInputStreamResource(bytes.toArray)

  private def bytesToInputStreamResource(bytes: Array[Byte]): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable:
      SyncIO:
        new ByteArrayInputStream(bytes): InputStream

  private def base64ToStreamResource(base64: String): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable:
      SyncIO[InputStream]:
        try new ByteArrayInputStream(Base64.getMimeDecoder.decode(base64))
        catch case e: IllegalArgumentException =>
          throw new IllegalArgumentException(s"Error in Base64 encoded data: ${e.getMessage}", e)

  object syntax:
    implicit final class RichF[F[_], A](private val underlying: F[A]) extends AnyVal:
      /** Compiles iff A == B or A extends B. */
      inline def requireElementType[B >: A]: F[A] =
        underlying

    extension[A](underlying: IO[A])
      def logAndIgnoreError(what: => String): IO[Unit] =
        underlying.void.handleError: t =>
          IO(logger.error(s"$what => ${t.toStringWithCauses}", t.nullIfNoStackTrace))

      def logWhenMethodTakesLonger(using sourcecode.Enclosing): IO[A] =
        Worry.Default.logWhenMethodTakesLonger:
          underlying

      def logWhenItTakesLonger(using sourcecode.Enclosing): IO[A] =
        Worry.Default.logWhenItTakesLonger:
          underlying

      def logWhenItTakesLonger(what: => String, worry: Worry = Worry.Default): IO[A] =
        worry.logWhenItTakesLonger(what):
          underlying

      def logWhenItTakesLonger(worry: Worry)(
        onDelayedOrCompleted: (Option[OutcomeIO[A]], FiniteDuration, LogLevel, String) => IO[String])
      : IO[A] =
        worry.logWhenItTakesLonger_(underlying):
          onDelayedOrCompleted

      /** When `this` takes longer than `duration` then call `thenDo` once. */
      @deprecated("Use whenItTakesLongerThan", "v2.7")
      def whenItTakesLonger(duration: FiniteDuration)(thenDo: IO[Unit]): IO[A] =
        whenItTakesLongerThan(duration)(thenDo)

      /** When `this` takes longer than `duration` then call `thenDo` once. */
      def whenItTakesLongerThan(duration: FiniteDuration)(thenDo: IO[Unit]): IO[A] =
        if duration.isZeroOrBelow then
          underlying
        else
          whenItTakesLonger(duration :: ZeroDuration :: Nil)(_ => thenDo)

      /** As long as `this` has not completed, call `thenDo` after each of `durations` .
       *
       * @param durations if empty then `thenDo` will not be called.
       *                  The last entry is repeated until `this` completes.
       *                  A zero or negative duration terminates calling of `thenDo`.
       * @param thenDo    A function which gets the elapsed time since start as argument. */
      def whenItTakesLonger(durations: IterableOnce[FiniteDuration] = Worry.Default.durations)
        (thenDo: FiniteDuration => IO[Unit])
      : IO[A] =
        val durationIterator = durations.iterator
        if durationIterator.isEmpty then
          underlying
        else
          CatsDeadline.now.flatMap: since =>
            ZeroDuration
              .tailRecM: lastDuration =>
                val d = durationIterator.nextOption() getOrElse lastDuration
                if !d.isNegative then
                  IO.sleep(d)
                    .*>(since.elapsed)
                    .flatMap(thenDo)
                    .as(Left(d))
                else
                  IO.right(())
              .background.surround:
                underlying


    implicit final class RichResource[F[_], A](private val resource: Resource[F, A])
    extends AnyVal:
      def void: Resource[F, Unit] =
        resource.as(())

      def as[B](b: B): Resource[F, B] =
        resource.map(_ => b)

      def toAllocated[G[x] >: F[x], B >: A](
        using MonadCancel[F, Throwable], UnsafeMemoizable[G], Tag[B])
      : G[Allocated[G, B]] =
        resource.allocated[B].map(Allocated.fromPair(_))

      def toLabeledAllocated[G[x] >: F[x], B >: A](label: String)
        (using MonadCancel[F, Throwable], UnsafeMemoizable[G])
      : G[Allocated[G, B]] =
        resource.allocated[B]
          .map: (b, release) =>
            new Allocated(b, release, label = label)

      def toAllocatedResource[G[x] >: F[x], B >: A: Tag](
        using G: Sync[G], F: MonadCancel[F, Throwable], g: UnsafeMemoizable[G])
      : Resource[G, Allocated[G, B]] =
        Resource.suspend(
          resource
            .toAllocated[G, B]
            .map(_.toSingleUseResource))


    extension [F[_], A](fa: F[A])
      def traverseFlat[G[_], B](f: A => G[F[B]])(using Traverse[F], FlatMap[F], Applicative[G])
      : G[F[B]] =
        fa.traverse(f).map(_.flatten)

    extension [L, R](view: View[Either[L, R]])
      /** A fail-fast specialization of sequence for View: */
      def sequence: Either[L, Vector[R]] =
        val builder = Vector.newBuilder[R]
        val it = view.iterator
        while it.hasNext do
          it.next() match
            case Left(l) => return Left(l)
            case Right(r) => builder += r
        Right(builder.result())

    extension [A](view: View[A])
      /** A fail-fast specialization of traverse for View: */
      def traverse[L, R](f: A => Either[L, R]): Either[L, Vector[R]] =
        val builder = Vector.newBuilder[R]
        val it = view.iterator
        while it.hasNext do
          f(it.next()) match
            case Left(l) => return Left(l)
            case Right(r) => builder += r
        Right(builder.result())

    extension [A](nonEmptyList: NonEmptyList[A])
      def view: View[A] = nonEmptyList.toList.view

      def mkString: String =
        nonEmptyList.toList.mkString

      def mkString(separator: String): String =
        nonEmptyList.toList.mkString(separator)

      def mkString(start: String, separator: String, end: String): String =
        nonEmptyList.toList.mkString(start, separator, end)

    extension [A](nonEmptyVector: NonEmptyVector[A])
      def view: View[A] = nonEmptyVector.toVector.view

  implicit final class RichThrowableValidated[E <: Throwable, A](private val underlying: Validated[E, A]) extends AnyVal:
    def orThrow: A =
      underlying.valueOr(t => throw t.appendCurrentStackTrace)

  implicit final class RichProblemValidated[E <: Problem, A](private val underlying: Validated[E, A]) extends AnyVal:
    def orThrow: A =
      underlying.valueOr(problem => throw problem.throwable.appendCurrentStackTrace)


  implicit final class RichNonEmptyListCompanion(private val x: NonEmptyList.type)
  extends AnyVal:
    def unsafe[A: Tag](seq: Seq[A]): NonEmptyList[A] =
      checked(seq).orThrow

    def checked[A: Tag](seq: Seq[A]): Checked[NonEmptyList[A]] =
      if seq.isEmpty then
        Left(Problem(s"Cannot create NonEmptyList[${Tag[A].tag.longNameWithPrefix}] from empty sequence"))
      else
        Right(NonEmptyList(seq.head, seq.toList.tail))

    def fromSeq[A](seq: Seq[A]): Option[NonEmptyList[A]] =
      NonEmptyList.fromList(seq.toList)

  implicit final class RichNonEmptySeqCompanion(private val x: NonEmptySeq.type)
  extends AnyVal:
    def unsafe[A: Tag](seq: Seq[A]): NonEmptySeq[A] =
      checked(seq).orThrow

    def checked[A: Tag](seq: Seq[A]): Checked[NonEmptySeq[A]] =
      if seq.isEmpty then
        Left(Problem(s"Cannot create NonEmptySeq[${Tag[A].tag.longNameWithPrefix}] from empty sequence"))
      else
        Right(NonEmptySeq(seq.head, seq.tail))

  def pureFiberIO[A](a: A): FiberIO[A] =
    PureFiberIO(a)


  /** Fiber immediately returns the given value, not cancelable. */
  final class PureFiber[F[_], E, A](a: A)(using F: Applicative[F])
    extends Fiber[F, E, A]:

    def cancel: F[Unit] =
      F.pure(())

    def join: F[Outcome[F, E, A]] =
      F.pure(Outcome.Succeeded(F.pure(a)))


  type PureFiberIO[A] = PureFiber[IO, Throwable, A]
  object PureFiberIO:
    val unit: FiberIO[Unit] = PureFiberIO(())

    def apply[A](a: A): FiberIO[A] =
      PureFiber(a)


  private val canceledFiberIO_ : FiberIO[Unit] = new CanceledFiber[IO, Throwable, Unit]

  def canceledFiberIO[A]: FiberIO[A] =
    canceledFiberIO_.asInstanceOf[FiberIO[A]]

  final class CanceledFiber[F[_], E, A](using F: Applicative[F])
    extends Fiber[F, E, A]:

    def cancel: F[Unit] =
      F.unit

    def join: F[Outcome[F, E, A]] =
      F.pure(Outcome.Canceled())
