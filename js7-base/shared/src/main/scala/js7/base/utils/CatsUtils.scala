package js7.base.utils

import cats.data.{NonEmptyList, NonEmptySeq, Validated}
import cats.effect.{Fiber, FiberIO, IO, MonadCancel, Outcome, OutcomeIO, Resource, Sync, SyncIO}
import cats.kernel.Monoid
import cats.syntax.all.*
import cats.{Applicative, Functor}
import fs2.{Pure, Stream}
import izumi.reflect.Tag
import java.io.{ByteArrayInputStream, InputStream}
import java.util.Base64
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.catsutils.{CatsDeadline, UnsafeMemoizable}
import js7.base.fs2utils.StreamExtensions.repeatLast
import js7.base.log.{LogLevel, Logger}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.*
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
object CatsUtils:

  type Nel[+A] = NonEmptyList[A]
  val Nel: NonEmptyList.type = NonEmptyList

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
      @inline def containsType[B >: A]: F[A] =
        underlying

    extension[A](underlying: IO[A])
      def logAndIgnoreError(what: => String): IO[Unit] =
        underlying.void.handleError: t =>
          IO(logger.error(s"$what => ${t.toStringWithCauses}", t.nullIfNoStackTrace))

      def logWhenItTakesLonger(using enclosing: sourcecode.Enclosing): IO[A] =
        logWhenItTakesLonger2("in", "continues", enclosing.value)

      def logWhenItTakesLonger(what: => String, worry: Worry = Worry.Default)
      : IO[A] =
        logWhenItTakesLonger2("for", "completed", what, worry)

      private def logWhenItTakesLonger2(preposition: String, completed: String, what: => String,
        worry: Worry = Worry.Default)
      : IO[A] =
        worry.logWhenItTakesLonger(preposition, completed, what, underlying)

      def logWhenItTakesLonger(worry: Worry)(
        onDelayedOrCompleted: (Option[OutcomeIO[A]], FiniteDuration, LogLevel, String) => IO[String])
      : IO[A] =
        worry.logWhenItTakesLonger(underlying)(onDelayedOrCompleted)

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
                if d.isPositive then
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
        using Functor[F], MonadCancel[F, Throwable], UnsafeMemoizable[G], Tag[B])
      : G[Allocated[G, B]] =
        resource.allocated[B].map(Allocated.fromPair(_))

      def toLabeledAllocated[G[x] >: F[x], B >: A](label: String)
        (using Functor[F], MonadCancel[F, Throwable], UnsafeMemoizable[G])
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

  // Scala 2.13 does not allow add a method to an AnyVal class
  def continueWithLast[A](seq: NonEmptySeq[A]): Iterator[A] =
    val last = seq.last
    seq.iterator ++ Iterator.continually(last)

  // Scala 2.13 does not allow add a method to an AnyVal class
  def continueWithLast[A](seq: NonEmptyList[A]): Iterator[A] =
    val last = seq.last
    seq.iterator ++ Iterator.continually(last)

  def continueWithLast[A](head: A, next: A, tail: A*): Iterator[A] =
    continueWithLast(NonEmptySeq.fromSeqUnsafe(Seq(head, next) ++ tail))

  def repeatLast[A](seq: Seq[A]): LazyList[A] =
    if seq.isEmpty then
      LazyList.empty
    else
      val last = seq.last
      seq ++: LazyList.continually(last)

  def continueWithLastAsStream[A](seq: Iterable[A]): Stream[Pure, A] =
    Stream.iterable(seq).repeatLast

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
