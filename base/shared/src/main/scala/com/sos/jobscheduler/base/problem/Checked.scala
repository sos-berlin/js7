package com.sos.jobscheduler.base.problem

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, HCursor, ObjectEncoder}
import scala.concurrent.Future
import scala.language.higherKinds
import scala.reflect.ClassTag
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
      case Some(o) ⇒ Valid(o)
      case None ⇒ Invalid(problem)
    }

  def fromTry[A](tried: Try[A]): Checked[A] =
    tried match {
      case Success(o) ⇒ Valid(o)
      case Failure(t) ⇒ Invalid(Problem.pure(t))
    }

  def catchNonFatal[A](f: ⇒ A): Checked[A] =
    try Valid(f)
    catch {
      case NonFatal(t) ⇒ Invalid(Problem.pure(t))
    }

  //implicit def checkedEq[A: Eq]: Eq[Checked[A]] = (x, y) ⇒
  //  (x, y) match {
  //    case (Valid(xx), Valid(yy)) ⇒ xx === yy
  //    case (Invalid(xx), Invalid(yy)) ⇒ xx === yy
  //    case _ ⇒ false
  //  }

  /** `Checked[A[V] ] => A[Checked[V] ]`. */
  def evert[A[_], V](checked: Checked[A[V]])(implicit A: Applicative[A]): A[Checked[V]] =
    checked match {
      case Valid(a) ⇒ A.map(a)(Valid.apply)
      case o @ Invalid(_) ⇒ A.pure(o)
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
    def >>=[B](f: A ⇒ Checked[B]): Checked[B] =
      underlying andThen f

    /** `Checked` has `flatMap` but is not a `cats.FlatMap`.
      * See documentation of `Validated#andThen`.
      * This may be impure but allows for-comprehension. */
    def flatMap[B](f: A ⇒ Checked[B]): Checked[B] =
      underlying andThen f

    def withProblemKey(key: Any): Checked[A] =
      mapProblem (_ withKey key)

    def mapProblem(f: Problem ⇒ Problem): Checked[A] =
      underlying match {
        case _: Valid[A] ⇒ underlying
        case Invalid(problem) ⇒ Invalid(f(problem))
      }

    /** Converts the `Checked` into an `Option`, executing the `sideEffect` if `Invalid`.
      */
    def onProblem(sideEffect: Problem ⇒ Unit): Option[A] =
      underlying match {
        case Invalid(problem) ⇒
          sideEffect(problem)
          None
        case Valid(a) ⇒  Some(a)
      }

    def toEitherThrowable: Either[Throwable, A] =
      underlying match {
        case Invalid(problem) ⇒ Left(problem.throwable)
        case Valid(o) ⇒ Right(o)
      }

    def toFuture: Future[A] =
      underlying match {
        case Valid(o) ⇒ Future.successful(o)
        case Invalid(problem) ⇒ Future.failed(problem.throwable)
      }

    def orThrow: A = underlying match {
      case Valid(a) ⇒ a
      case Invalid(problem) ⇒ throw problem.throwable.appendCurrentStackTrace
    }
  }

  implicit final class CheckedFlattenOps[A](private val underlying: Checked[Checked[A]]) extends AnyVal {
    def flatten: Checked[A] = underlying.flatMap(identity)
  }

  implicit final class CheckedOption[A](private val underlying: Option[A]) extends AnyVal {
    def toChecked(problem: ⇒ Problem): Checked[A] =
      underlying match {
        case Some(a) ⇒ Valid(a)
        case None ⇒ Invalid(problem)
      }
  }

  object implicits {
    //implicit def checkedJsonCodec[A: ObjectEncoder: Decoder: ClassTag]: TypedJsonCodec[Checked[A]] =
    //  TypedJsonCodec[Checked[A]](
    //    Subtype[Valid[A]](
    //      encoder = ObjectEncoder.instance[Valid[A]](_.a.asJsonObject),
    //      decoder = Decoder.instance(_.as[A] map Valid.apply)),
    //    Subtype[Invalid[Problem]](
    //      encoder = ObjectEncoder.instance[Invalid[Problem]](_.e.asJsonObject),
    //      decoder = Decoder.instance(_.as[Problem] map Invalid.apply)))

    implicit def checkedJsonCodec[A: TypedJsonCodec: ClassTag]: CirceObjectCodec[Checked[A]] =
      new ObjectEncoder[Checked[A]] with  Decoder[Checked[A]] {
        def encodeObject(checked: Checked[A]) = checked match {
          case Valid(a) ⇒ a.asJsonObject
          case Invalid(problem) ⇒ Problem.typedJsonEncoder.encodeObject(problem)
        }

        def apply(c: HCursor) =
          for {
            typ ← c.get[String](TypedJsonCodec.TypeFieldName)
            result ← typ match {
              case "Problem" ⇒ c.as[Problem] map Invalid.apply
              case _ ⇒ c.as[A] map Valid.apply
            }
          } yield result
      }
  }
}
