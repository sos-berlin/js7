package com.sos.jobscheduler.base.problem

import cats.data.Validated.Invalid
import cats.syntax.semigroup._
import cats.{Eq, Semigroup}
import com.sos.jobscheduler.base.problem.Problem._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import io.circe.{Decoder, Json, JsonObject, ObjectEncoder}
import scala.collection.immutable.Iterable
import scala.language.implicitConversions
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
sealed trait Problem
{
  def throwable: Throwable

  def throwableOption: Option[Throwable] = None

  def withKey(key: Any): Problem = withPrefix(s"Problem with '$key':")

  def withPrefix(prefix: String): Problem = Problem(prefix) |+| this

  def head: Problem = this

  override def equals(o: Any) = o match {
    case o: Problem ⇒ toString == o.toString
    case _ ⇒ false
  }

  override def toString: String
}

object Problem
{
  implicit def toInvalid[A](problem: Problem): Invalid[Problem] =
    Invalid(problem)

  def apply(messageFunction: ⇒ String): Problem =
    new Lazy(messageFunction)

  def fromEager(message: String): Problem =
    apply(message)

  def fromEagerThrowable(throwable: Throwable): Problem =
    fromLazyThrowable(throwable)

  def fromLazyThrowable(throwable: ⇒ Throwable): Problem =
    new FromThrowable(() ⇒ throwable)

  sealed trait HasMessage extends Problem {
    def message: String
  }

  class Lazy protected[problem](messageFunction: ⇒ String) extends HasMessage {
    final lazy val message = {
      messageFunction match {
        case null ⇒ "A problem occurred (null)"
        case "" ⇒ "A problem occurred (no message)"
        case o ⇒ o
      }
    }

    final def throwable = new ProblemException(message)

    override final def withPrefix(prefix: String) = new Lazy(normalizePrefix(prefix) + message)

    override def hashCode = message.hashCode

    override def toString = message
  }

  def set(problems: String*): Multiple =
    multiple(problems.toSet)

  def multiple(problems: String*): Multiple =
    multiple(problems.toImmutableSeq)

  def multiple(problems: Iterable[String]): Multiple =
    Multiple(problems.map(o ⇒ new Lazy(o)))

  final case class Multiple private[problem](problems: Iterable[Lazy]) extends HasMessage {
    require(problems.nonEmpty)

    def throwable = new ProblemException(toString)

    lazy val message = problems map (_.toString) reduce combineMessages

    override def head = problems.head

    override def equals(o: Any) = o match {
      case o: Multiple ⇒
        (problems, o.problems) match {
          case (problems: Set[Lazy], _) ⇒ problems == o.problems.toSet  // Ignore ordering (used in tests)
          case (_, o: Set[Lazy])        ⇒ problems.toSet == o           // Ignore ordering (used in tests)
          case _                        ⇒ problems == o.problems
        }
        case _ ⇒ super.equals(o)
      }

    override def toString = message
  }

  private class FromThrowable(throwableFunction: () ⇒ Throwable) extends Problem {
    private lazy val throwable_ = throwableFunction()

    def throwable = throwable_

    override def throwableOption = Some(throwable_)

    override def toString = throwable_.toStringWithCauses
  }

  implicit val semigroup: Semigroup[Problem] = {
    case (a: Problem, b: FromThrowable) ⇒
      Problem.fromLazyThrowable(new ProblemException(a.toString, b.throwable) with NoStackTrace)

    case (a: FromThrowable, b: Problem) ⇒
      Problem.fromLazyThrowable {
        val t = new ProblemException(a.toString, b.throwable) with NoStackTrace
        t.setStackTrace(a.throwable.getStackTrace)
        t
      }

    case (a: Lazy, b: Lazy) ⇒
      Multiple(a :: b :: Nil)

    case (a: Lazy, b: Multiple) ⇒
      Multiple(a +: b.problems.toVector)

    case (a: Multiple, b: Lazy) ⇒
      Multiple(a.problems.toVector :+ b)

    case (a: Multiple, b: Problem) ⇒
      Multiple(a.problems.toVector :+ new Lazy(b.toString))  // TODO If b is FromThrowable, then b.throwable is lost
  }

  implicit val eqv: Eq[Problem] = Eq.fromUniversalEquals[Problem]

  private def combineMessages(a: String, b: String) =
    if (b.trim.isEmpty)
      a
    else
      normalizePrefix(a) + b

  private def normalizePrefix(prefix: String): String =
    if (prefix matches ".*[:-] *")
      if (prefix endsWith " ") prefix else prefix + " "
    else if (prefix.trim.isEmpty && prefix.endsWith(" "))
      prefix
    else
      prefix + "\n & "

  implicit val jsonEncoder: ObjectEncoder[Problem] =
    problem ⇒ JsonObject(
      "TYPE" → Json.fromString("Problem"),
      "message" → Json.fromString(problem.toString))

  implicit val jsonDecoder: Decoder[Problem] =
    c ⇒ for (message ← c.get[String]("message")) yield
      Problem.fromEager(message)
}
