package js7.base.problem

import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.generic.Completed
import js7.base.problem.Problem._
import js7.base.utils.ScalaUtils.syntax._
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object Checked
{
  val unit = Checked(())
  val completed = Checked(Completed)
  private val logger = scribe.Logger[this.type]

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
      case Failure(t) => Left(Problem.fromThrowable(t))
      case Success(o) => Right(o)
    }

  def check[A](predicate: Boolean, a: => A, problem: => Problem): Checked[A] =
    if (predicate) Right(a) else Left(problem)

  def catchNonFatal[A](f: => A): Checked[A] =
    try Right(f)
    catch {
      case NonFatal(t) =>
        for (t <- t.ifStackTrace) logger.debug(s"Checked.catchNonFatal: ${t.toStringWithCauses}", t)
        Left(Problem.fromThrowable(t))
    }

  def catchProblem[A](f: => A): Checked[A] =
    try Right(f)
    catch {
      case e: ProblemException => Left(e.problem)
    }

  implicit final class Ops[A](private val underlying: Checked[A]) extends AnyVal
  {
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

    def toUnit: Either[Problem, Unit] =
      underlying match {
        case Right(_) => Right(())
        case Left(problem) => Left(problem)
      }

    def toCompleted: Either[Problem, Completed] =
      underlying match {
        case Right(_) => Right(Completed)
        case Left(problem) => Left(problem)
      }

    def orThrow: A =
      underlying
        .left.map(_.throwable)
        .orThrow

    def orThrowWithoutStacktrace: A =
      underlying.left.map(_.throwable).orThrow
  }

  implicit final class RichCheckedIterable[A](private val underlying: IterableOnce[Checked[A]]) extends AnyVal {
    def traverseAndCombineProblems[B](f: A => Checked[B]): Checked[Seq[B]] = {
      val rightBuilder = new VectorBuilder[B]
      val leftBuilder = mutable.Buffer[Problem]()
      underlying.iterator.foreach(
        _.flatMap(f) match {
          case Left(problem) => leftBuilder += problem
          case Right(b) => if (leftBuilder.isEmpty) rightBuilder += b
        })
      Problem.combineAllOption(leftBuilder) match {
        case Some(problem) => Left(problem)
        case None => Right(rightBuilder.result())
      }
    }

    def combineProblems: Checked[Seq[A]] = {
      val rightBuilder = new VectorBuilder[A]
      val leftBuilder = mutable.Buffer[Problem]()
      underlying.iterator.foreach {
        case Left(problem) => leftBuilder += problem
        case Right(b) => if (leftBuilder.isEmpty) rightBuilder += b
      }
      Problem.combineAllOption(leftBuilder) match {
        case Some(problem) => Left(problem)
        case None => Right(rightBuilder.result())
      }
    }
  }

  implicit final class CheckedOption[A](private val underlying: Option[A]) extends AnyVal {
    def toChecked(problem: => Problem): Checked[A] =
      underlying match {
        case None => Left(problem)
        case Some(a) => Right(a)
      }
  }

  implicit final class FailFastMap[A](private val underlying: IterableOnce[A]) {
    /** Like map(f).sequence, but fails fast. `L` does not accumulate. */
    def failFastMap[B, L](f: A => Either[L, B]): Either[L, Seq[B]] = {
      val builder = new VectorBuilder[B]
      var failed: Left[L, B] = null
      val it = underlying.iterator
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

  object implicits
  {
    implicit def checkedJsonEncoder[A](implicit A: Encoder[A]): Encoder[Checked[A]] = {
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
