package js7.base.utils

import cats.Functor
import cats.data.{NonEmptyList, NonEmptySeq, Validated}
import cats.effect.concurrent.Deferred
import cats.effect.{BracketThrow, Resource, Sync, SyncIO, Timer}
import cats.kernel.Monoid
import cats.syntax.all.*
import izumi.reflect.Tag
import java.io.{ByteArrayInputStream, InputStream}
import java.util.Base64
import js7.base.catsutils.UnsafeMemoizable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.*
import monix.eval.{Fiber, Task}
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
object CatsUtils
{
  type Nel[+A] = NonEmptyList[A]
  val Nel = NonEmptyList

  def combine[A: Monoid](as: A*): A =
    as.combineAll

  def completedFiber[A](a: A): Fiber[A] =
    Fiber(Task.pure(a), Task.unit)

  private def bytesToInputStreamResource(bytes: collection.Seq[Byte]): Resource[SyncIO, InputStream] =
    bytesToInputStreamResource(bytes.toArray)

  private def bytesToInputStreamResource(bytes: Array[Byte]): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO { new ByteArrayInputStream(bytes): InputStream })

  private def base64ToStreamResource(base64: String): Resource[SyncIO, InputStream] =
    Resource.fromAutoCloseable(SyncIO[InputStream] {
      try new ByteArrayInputStream(Base64.getMimeDecoder.decode(base64))
      catch { case e: IllegalArgumentException =>
        throw new IllegalArgumentException(s"Error in Base64 encoded data: ${e.getMessage}", e)
      }
    })

  object syntax {
    implicit final class RichF[F[_], A](private val underlying: F[A]) extends AnyVal {
      /** Compiles iff A == B or A extends B. */
      @inline def containsType[B >: A]: F[A] =
        underlying
    }

    implicit final class RichTimer[F[_]](private val timer: Timer[F]) extends AnyVal {
      def now(implicit F: Functor[F]): F[Deadline] =
        for (nanos <- timer.clock.monotonic(NANOSECONDS)) yield
          Deadline(Duration(nanos, NANOSECONDS))
    }

    implicit final class RichSyncTimer(private val timer: Timer[SyncIO]) extends AnyVal {
      def unsafeNow(): FiniteDuration =
        Duration(
          timer.clock.monotonic(NANOSECONDS).unsafeRunSync(),
          NANOSECONDS)
    }

    implicit final class RichResource[F[_], A](private val resource: Resource[F, A])
    extends AnyVal {
      def toAllocated[G[x] >: F[x], B >: A](using BracketThrow[G], UnsafeMemoizable[G], Tag[B])
      : G[Allocated[G, B]] =
        resource.allocated[G, B].map(Allocated.fromPair(_))

      def toLabeledAllocated[G[x] >: F[x], B >: A](label: String)(using BracketThrow[G], UnsafeMemoizable[G])
      : G[Allocated[G, B]] =
        resource.allocated[G, B]
          .map { case (b, release) => new Allocated(b, release, label = label) }

      def toAllocatedResource[G[x] >: F[x], B >: A: Tag](implicit G: Sync[G], g: UnsafeMemoizable[G])
      : Resource[G, Allocated[G, B]] =
        Resource.suspend(
          resource
            .toAllocated[G, B]
            .map(_.toSingleUseResource))
    }
  }

  implicit final class RichDeferred[F[_], A](private val deferred: Deferred[F, A]) extends AnyVal {
    /** For compatibility with Cats Effect 3. */
    def complete3(a: A)(implicit F: Sync[F]): F[Boolean] =
      deferred.complete(a).attempt.map(_.isRight)
  }

  implicit final class RichThrowableValidated[E <: Throwable, A](private val underlying: Validated[E, A]) extends AnyVal
  {
    def orThrow: A =
      underlying.valueOr(t => throw t.appendCurrentStackTrace)
  }

  implicit final class RichProblemValidated[E <: Problem, A](private val underlying: Validated[E, A]) extends AnyVal
  {
    def orThrow: A =
      underlying.valueOr(problem => throw problem.throwable.appendCurrentStackTrace)
  }

  implicit final class RichNonEmptyListCompanion(private val x: NonEmptyList.type)
  extends AnyVal {
    def unsafe[A: Tag](seq: Seq[A]): NonEmptyList[A] =
      checked(seq).orThrow

    def checked[A: Tag](seq: Seq[A]): Checked[NonEmptyList[A]] =
      if (seq.isEmpty)
        Left(Problem(s"Cannot create NonEmptyList[${Tag[A].tag.longName}] from empty sequence"))
      else
        Right(NonEmptyList(seq.head, seq.toList.tail))
  }

  implicit final class RichNonEmptySeqCompanion(private val x: NonEmptySeq.type)
  extends AnyVal {
    def unsafe[A: Tag](seq: Seq[A]): NonEmptySeq[A] =
      checked(seq).orThrow

    def checked[A: Tag](seq: Seq[A]): Checked[NonEmptySeq[A]] =
      if (seq.isEmpty)
        Left(Problem(s"Cannot create NonEmptySeq[${Tag[A].tag.longName}] from empty sequence"))
      else
        Right(NonEmptySeq(seq.head, seq.tail))
  }

  // Scala 2.13 does not allow add a method to an AnyVal class
  def continueWithLast[A](seq: NonEmptySeq[A]): Iterator[A] = {
    val last = seq.last
    seq.iterator ++ Iterator.continually(last)
  }

  // Scala 2.13 does not allow add a method to an AnyVal class
  def continueWithLast[A](seq: NonEmptyList[A]): Iterator[A] = {
    val last = seq.last
    seq.iterator ++ Iterator.continually(last)
  }

  def continueWithLast[A](head: A, next: A, tail: A*): Iterator[A] =
    continueWithLast(NonEmptySeq.fromSeqUnsafe(Seq(head, next) ++ tail))

  def repeatLast[A](seq: Seq[A]): LazyList[A] =
    if (seq.isEmpty)
      LazyList.empty
    else {
      val last = seq.last
      seq ++: LazyList.continually(last)
    }

  //def repeatLast[A](seq: NonEmptySeq[A]): LazyList[A] = {
  //  val last = seq.last
  //  seq.toSeq ++: LazyList.continually(last)
  //}
}
