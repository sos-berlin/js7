package com.sos.jobscheduler.base.problem

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.eq._
import cats.{Eq, Monad}
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object Checked
{
  def apply[A](a: A): Checked[A] = Valid(a)

  def fromOption[A](a: Option[A], problem: Problem) =
    a match {
      case Some(o) ⇒ Valid(o)
      case None ⇒ Invalid(problem)
    }

  def catchNonFatal[A](f: ⇒ A): Checked[A] =
    try Valid(f)
    catch {
      case NonFatal(t) ⇒ Invalid(Problem.fromEagerThrowable(t))
    }

  //def problem(lazyMessage: ⇒ String): Invalid[Problem] =
  //  Invalid(Problem(lazyMessage))
  //
  //def eagerThrowable(throwable: Throwable): Invalid[Problem] =
  //  Invalid(Problem.fromEagerThrowable(throwable))
  //
  //def lazyThrowable(throwable: ⇒ Throwable): Invalid[Problem] =
  //  Invalid(Problem.fromLazyThrowable(throwable))

  implicit val monad: Monad[Checked] = new Monad[Checked] {
    def pure[A](a: A) = Checked(a)

    def flatMap[A, B](fa: Checked[A])(f: A ⇒ Checked[B]) =
      fa match {
        case Valid(a) ⇒ f(a)
        case Invalid(o) ⇒ Invalid(o)
      }

    def tailRecM[A, B](a: A)(f: A ⇒ Checked[Either[A, B]]): Checked[B] = {
      @tailrec def loop(a: A): Checked[B] = f(a) match {
        case Valid(Left(a2)) ⇒ loop(a2)
        case Valid(Right(b)) ⇒ Valid(b)
        case Invalid(problem) ⇒ Invalid(problem)
      }
      loop(a)
    }
  }

  implicit def _eq[A: Eq]: Eq[Checked[A]] = (x, y) ⇒
    (x, y) match {
      case (Valid(xx), Valid(yy)) ⇒ xx === yy
      case (Invalid(xx), Invalid(yy)) ⇒ xx == yy
      case _ ⇒ false
    }

  object ops {
    implicit class RichChecked[A](val underlying: Checked[A]) extends AnyVal
    {
      def flatMap[B](f: A ⇒ Checked[B]): Checked[B] = monad.flatMap(underlying)(f)

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

      def toFuture: Future[A] =
        underlying match {
          case Valid(o) ⇒ Future.successful(o)
          case Invalid(problem) ⇒ Future.failed(problem.throwable)
        }

      def force: A = underlying match {
        case Valid(a) ⇒ a
        case Invalid(problem) ⇒ throw problem.throwable.appendCurrentStackTrace
      }
    }

    implicit class RichOption[A](val underlying: Option[A]) extends AnyVal {
      def toChecked(problem: ⇒ Problem): Checked[A] =
        underlying match {
          case Some(a) ⇒ Valid(a)
          case None ⇒ Invalid(problem)
        }
    }
  }
}
