package com.sos.jobscheduler.base.problem

import cats.Applicative
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import io.circe.{Decoder, Encoder, Json}
import scala.collection.immutable.{Seq, VectorBuilder}
import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object Checked
{
  val unit = Checked(())
  val completed = Checked(Completed)

  def apply[A](a: A): Checked[A] = Valid(a)

  def fromOption[A](a: Option[A], problem: Problem): Checked[A] =
    a match {
      case Some(o) => Valid(o)
      case None => Invalid(problem)
    }

  def flattenTryChecked[A](tryChecked: Try[Checked[A]]): Checked[A] =
    fromTry(tryChecked).flatten

  def fromTry[A](tried: Try[A]): Checked[A] =
    tried match {
      case Success(o) => Valid(o)
      case Failure(t) => Invalid(Problem.pure(t))
    }

  final def invalidIf(predicate: Boolean, problem: => Problem): Checked[Unit] =
    if (predicate) Invalid(problem) else unit

  final def cond[A](predicate: Boolean, a: => A, problem: => Problem): Checked[A] =
    Validated.cond(predicate, a, problem)

  def ifOr(predicate: Boolean, problem: Problem): Checked[Unit] =
    if (predicate) unit else Invalid(problem)

  def catchNonFatal[A](f: => A): Checked[A] =
    try Valid(f)
    catch {
      case NonFatal(t) => Invalid(Problem.pure(t))
    }

  def catchProblem[A](f: => A): Checked[A] =
    try Valid(f)
    catch {
      case e: ProblemException => Invalid(e.problem)
    }

  //implicit def checkedEq[A: Eq]: Eq[Checked[A]] = (x, y) =>
  //  (x, y) match {
  //    case (Valid(xx), Valid(yy)) => xx === yy
  //    case (Invalid(xx), Invalid(yy)) => xx === yy
  //    case _ => false
  //  }

  /** `Checked[A[V] ] => A[Checked[V] ]`. */
  def evert[A[_], V](checked: Checked[A[V]])(implicit A: Applicative[A]): A[Checked[V]] =
    checked match {
      case Valid(a) => A.map(a)(Valid.apply)
      case o @ Invalid(_) => A.pure(o)
    }

  implicit final class EvertChecked[A[_], V](private val underlying: Checked[A[V]]) extends AnyVal {
    /** `Checked[A[V]``] => A[Checked[V]``]`. */
    def evert(implicit A: Applicative[A]): A[Checked[V]] = Checked.evert(underlying)
  }

  implicit final class Ops[A](private val underlying: Checked[A]) extends AnyVal
  {
    /** `Checked` has `flatMap` but is not a `cats.FlatMap`.
      * See documentation of `Validated#andThen`.
      * This may be impure but allows for-comprehension. */
    def >>=[B](f: A => Checked[B]): Checked[B] =
      underlying andThen f

    /** `Checked` has `flatMap` but is not a `cats.FlatMap`.
      * See documentation of `Validated#andThen`.
      * This may be impure but allows for-comprehension. */
    def flatMap[B](f: A => Checked[B]): Checked[B] =
      underlying andThen f

    def withProblemKey(key: Any): Checked[A] =
      mapProblem (_ withKey key)

    def mapProblem(f: Problem => Problem): Checked[A] =
      underlying match {
        case _: Valid[A] => underlying
        case Invalid(problem) => Invalid(f(problem))
      }

    /** Converts the `Checked` into an `Option`, executing the `sideEffect` if `Invalid`.
      */
    def onProblem(sideEffect: Problem => Unit): Option[A] =
      underlying match {
        case Invalid(problem) =>
          sideEffect(problem)
          None
        case Valid(a) =>  Some(a)
      }

    def onProblemHandle[B >: A](f: Problem => B): B =
      underlying match {
        case Invalid(problem) => f(problem)
        case Valid(a) => a
      }

    def toEitherThrowable: Either[Throwable, A] =
      underlying match {
        case Invalid(problem) => Left(problem.throwable)
        case Valid(o) => Right(o)
      }

    def toTry: Try[A] =
      underlying match {
        case Invalid(problem) => Failure(problem.throwable)
        case Valid(o) => Success(o)
      }

    def toFuture: Future[A] =
      underlying match {
        case Valid(o) => Future.successful(o)
        case Invalid(problem) => Future.failed(problem.throwable)
      }

    def orThrow: A = underlying match {
      case Valid(a) => a
      case Invalid(problem) => throw problem.throwable.appendCurrentStackTrace
    }

    def orThrowWithoutStacktrace: A = underlying match {
      case Valid(a) => a
      case Invalid(problem) => throw problem.throwable
    }
  }

  implicit final class CheckedFlattenOps[A](private val underlying: Checked[Checked[A]]) extends AnyVal {
    def flatten: Checked[A] = underlying.flatMap(identity)
  }

  implicit final class CheckedOption[A](private val underlying: Option[A]) extends AnyVal {
    def toChecked(problem: => Problem): Checked[A] =
      underlying match {
        case Some(a) => Valid(a)
        case None => Invalid(problem)
      }
  }

  implicit final class FailFastMap[A](private val underlying: TraversableOnce[A]) {
    /** Like map(f).sequence, but fails fast. `E` does not accumulate. */
    def failFastMap[B, E](f: A => Validated[E, B]): Validated[E, Seq[B]] = {
      val builder = new VectorBuilder[B]
      var failed: Invalid[E] = null
      val it = underlying.toIterator
      while (failed == null && it.hasNext) {
        f(it.next()) match {
          case Valid(b) => builder += b
          case o @ Invalid(_) => failed = o
        }
      }
      failed match {
        case null => Valid(builder.result())
        case o => o
      }
    }
  }

  object implicits {
    implicit def checkedJsonEncoder[A](implicit A: Encoder[A]): Encoder[Checked[A]] =
      checked => checked match {
        case Valid(a) => A.apply(a)
        case Invalid(problem) => Json.fromJsonObject(Problem.typedJsonEncoder.encodeObject(problem))
      }

    implicit def checkedJsonDecoder[A: Decoder]: Decoder[Checked[A]] = c =>
      for {
        typ <- c.get[Option[String]](TypedJsonCodec.TypeFieldName)
        result <- typ match {
          case Some("Problem") => c.as[Problem] map Invalid.apply
          case _ => c.as[A] map Valid.apply
        }
      } yield result
  }
}
