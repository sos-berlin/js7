package js7.base.utils

import cats.data.{NonEmptyList, NonEmptySeq, Validated}
import cats.effect.{Resource, SyncIO}
import cats.kernel.Monoid
import cats.syntax.foldable.*
import izumi.reflect.Tag
import java.io.{ByteArrayInputStream, InputStream}
import java.util.Base64
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.StackTraces.*

/**
  * @author Joacim Zschimmer
  */
object CatsUtils
{
  type Nel[A] = NonEmptyList[A]
  val Nel = NonEmptyList

  def combine[A: Monoid](as: A*): A =
    as.combineAll

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
