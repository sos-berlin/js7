package com.sos.jobscheduler.base.problem

import cats.Semigroup
import cats.syntax.semigroup._
import com.sos.jobscheduler.base.problem.Problem._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
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

  override def equals(o: Any) = o match {
    case o: Problem ⇒ toString == o.toString
    case _ ⇒ false
  }
}

object Problem
{
  def apply(messageFunction: ⇒ String): Problem =
    new FromString(() ⇒ messageFunction)

  def fromEagerThrowable(throwable: Throwable): Problem =
    fromLazyThrowable(throwable)

  def fromLazyThrowable(throwable: ⇒ Throwable): Problem =
    new FromThrowable(() ⇒ throwable)

  sealed trait HasMessage extends Problem {
    def message: String
  }

  class FromString protected[problem](messageFunction: () ⇒ String) extends HasMessage {
    final lazy val message = messageFunction()

    def throwable = new ProblemException(message)

    override def withPrefix(prefix: String) = new FromString(() ⇒ normalizePrefix(prefix) + messageFunction())

    override def hashCode = message.hashCode

    override def toString = message
  }

  def set(problems: String*) = Multiple(problems.map(o ⇒ new FromString(() ⇒ o)).toSet)

  final case class Multiple private[problem](problems: Iterable[FromString]) extends HasMessage {
    require(problems.nonEmpty)

    def throwable = new ProblemException(toString)

    lazy val message = problems map (_.toString) reduce combineMessages

    override def equals(o: Any) = o match {
      case o: Multiple ⇒
        (problems, o.problems) match {
          case (problems: Set[FromString], _) ⇒ problems == o.problems.toSet  // Ignore ordering (used in tests)
          case (_, o: Set[FromString])        ⇒ problems.toSet == o           // Ignore ordering (used in tests)
          case _                              ⇒ problems == o.problems
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
    case (a: FromString, b: FromString) ⇒
      Multiple(a :: b :: Nil)

    case (a: FromString, b: Multiple) ⇒
      Multiple(a +: b.problems.toVector)

    case (a: Multiple, b: FromString) ⇒
      Multiple(a.problems.toVector :+ b)

    case (a: FromString, b: FromThrowable) ⇒
      Problem.fromLazyThrowable(new ProblemException(a.toString, b.throwable) with NoStackTrace)

    case (a: FromThrowable, b: Problem) ⇒
      Problem.fromLazyThrowable {
        val t = new ProblemException(a.toString, b.throwable) with NoStackTrace
        t.setStackTrace(a.throwable.getStackTrace)
        t
      }
  }

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
}
