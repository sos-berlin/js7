package com.sos.jobscheduler.base.problem

import cats.Applicative
import cats.instances.either._
import cats.syntax.flatMap._
import cats.syntax.semigroup._
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Problem._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import io.circe.{Decoder, Encoder, Json}
import scala.collection.immutable.{Iterable, Seq, VectorBuilder}
import scala.collection.mutable
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

  def apply[A](a: A): Checked[A] = Right(a)

  def fromOption[A](a: Option[A], problem: Problem): Checked[A] =
    a match {
      case None => Left(problem)
      case Some(o) => Right(o)
    }

  def flattenTryChecked[A](tryChecked: Try[Checked[A]]): Checked[A] =
    fromTry(tryChecked).flatten

  def fromTry[A](tried: Try[A]): Checked[A] =
    tried match {
      case Failure(t) => Left(Problem.pure(t))
      case Success(o) => Right(o)
    }

  final def invalidIf(predicate: Boolean, problem: => Problem): Checked[Unit] =
    if (predicate) Left(problem) else unit

  final def cond[A](predicate: Boolean, a: => A, problem: => Problem): Checked[A] =
    Either.cond(predicate, a, problem)

  def ifOr(predicate: Boolean, problem: Problem): Checked[Unit] =
    if (predicate) unit else Left(problem)

  def catchNonFatal[A](f: => A): Checked[A] =
    try Right(f)
    catch {
      case NonFatal(t) => Left(Problem.pure(t))
    }

  def catchProblem[A](f: => A): Checked[A] =
    try Right(f)
    catch {
      case e: ProblemException => Left(e.problem)
    }

  // Why does not Cats provide a `traverse` ???
  def traverse[F[_]: Applicative, A, B](checked: Checked[A])(f: A => F[B]): F[Checked[B]] =
    checked match {
      case Left(problem) => Applicative[F].pure(Left(problem))
      case Right(a) => Applicative[F].map(f(a))(Right.apply)
    }

  /** `Checked[F[A] ] => F[Checked[A] ]`. */
  def evert[F[_], A](checked: Checked[F[A]])(implicit A: Applicative[F]): F[Checked[A]] =
    checked match {
      case Left(problem) => A.pure(Left(problem))
      case Right(r) => A.map(r)(Right.apply)
    }

  implicit final class EvertChecked[F[_], A](private val underlying: Checked[F[A]]) extends AnyVal {
    /** `Checked[F[A]``] => F[Checked[A]``]`. */
    def evert(implicit A: Applicative[F]): F[Checked[A]] = Checked.evert(underlying)
  }

  implicit final class Ops[A](private val underlying: Checked[A]) extends AnyVal
  {
    def traverse[F[_]: Applicative, B](f: A => F[B]): F[Checked[B]] =
      Checked.traverse(underlying)(f)

    def withProblemKey(key: Any): Checked[A] =
      mapProblem (_ withKey key)

    def mapProblem(f: Problem => Problem): Checked[A] =
      underlying match {
        case Left(problem) => Left(f(problem))
        case _: Right[Problem, A] => underlying
      }

    /** Converts the `Checked` into an `Option`, executing the `sideEffect` if `Invalid`.
      */
    def onProblem(sideEffect: Problem => Unit): Option[A] =
      underlying match {
        case Left(problem) =>
          sideEffect(problem)
          None
        case Right(a) =>  Some(a)
      }

    def onProblemHandle[B >: A](f: Problem => B): B =
      underlying match {
        case Left(problem) => f(problem)
        case Right(a) => a
      }

    def toEitherThrowable: Either[Throwable, A] =
      underlying match {
        case Left(problem) => Left(problem.throwable)
        case Right(o) => Right(o)
      }

    /** Different from Either's `toTry`. */
    def asTry: Try[A] =
      underlying match {
        case Left(problem) => Failure(problem.throwable)
        case Right(o) => Success(o)
      }

    def toFuture: Future[A] =
      underlying match {
        case Left(problem) => Future.failed(problem.throwable)
        case Right(o) => Future.successful(o)
      }

    def orElse[B >: A](other: => Checked[B]): Checked[B] =
      underlying match {
        case Left(aProblem) =>
          other match {
            case Left(bProblem) => /*aProblem |+|*/ bProblem
            case Right(right) => Right(right)
          }
        case Right(right) => Right(right)
      }

    def valueOr[B >: A](orElse: Problem => B): B =
      underlying match {
        case Left(problem) => orElse(problem)
        case Right(value) => value
      }

    def toCompleted: Either[Problem, Completed] =
      underlying match {
        case Right(_) => Right(Completed)
        case Left(problem) => Left(problem)
      }

    def orThrow: A =
      orThrow(identity)

    def orThrow(toThrowable: Throwable => Throwable): A =
      underlying
        .left.map(_.throwable)
        .orThrow(toThrowable)

    def orThrowWithoutStacktrace: A =
      underlying.left.map(_.throwable).orThrow
  }

  implicit final class RichCheckedIterable[A](private val underlying: Iterable[Checked[A]]) extends AnyVal {
    def traverseAndCombineProblems[B](f: A => Checked[B]): Checked[Seq[B]] = {
      val rightBuilder = Vector.newBuilder[B]
      val leftBuilder = mutable.Buffer[Problem]()
      underlying.map(
        _.flatMap(f) match {
          case Left(problem) => leftBuilder += problem
          case Right(b) => if (leftBuilder.isEmpty) rightBuilder += b
        })
      if (leftBuilder.nonEmpty) Left(leftBuilder.toVector.reduce(_ |+| _))
      else Right(rightBuilder.result())
    }
  }

  implicit final class CheckedOption[A](private val underlying: Option[A]) extends AnyVal {
    def toChecked(problem: => Problem): Checked[A] =
      underlying match {
        case None => Left(problem)
        case Some(a) => Right(a)
      }
  }

  implicit final class FailFastMap[A](private val underlying: TraversableOnce[A]) {
    /** Like map(f).sequence, but fails fast. `L` does not accumulate. */
    def failFastMap[B, L](f: A => Either[L, B]): Either[L, Seq[B]] = {
      val builder = new VectorBuilder[B]
      var failed: Left[L, B] = null
      val it = underlying.toIterator
      while (failed == null && it.hasNext) {
        f(it.next()) match {
          case o @ Left(_) => failed = o
          case Right(b) => builder += b
        }
      }
      failed match {
        case null => Right(builder.result())
        case o => o.asInstanceOf[Left[L, Seq[B]]]
      }
    }
  }

  object implicits {
    implicit def checkedJsonEncoder[A](implicit A: Encoder[A]): Encoder[Checked[A]] =
      checked => checked match {
        case Left(problem) => Json.fromJsonObject(Problem.typedJsonEncoder.encodeObject(problem))
        case Right(a) => A.apply(a)
      }

    implicit def checkedJsonDecoder[A: Decoder]: Decoder[Checked[A]] = c =>
      for {
        typ <- c.get[Option[String]](TypedJsonCodec.TypeFieldName)
        result <- typ match {
          case Some("Problem") => c.as[Problem] map Left.apply
          case _ => c.as[A] map Right.apply
        }
      } yield result
  }
}
