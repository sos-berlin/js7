package com.sos.jobscheduler.base.problem

import cats.Semigroup
import cats.syntax.semigroup._
import com.sos.jobscheduler.base.problem.Problem._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
sealed trait Problem
{
  def throwable: Throwable

  def throwableOption: Option[Throwable] = None

  def withKey(key: Any): Problem = withPrefix(s"Problem with '$key':")

  def withPrefix(prefix: String): Problem = Problem(prefix) |+| this
}

object Problem
{
  def apply(messageFunction: ⇒ String): Problem =
    new FromString(() ⇒ messageFunction)

  def fromEagerThrowable(throwable: Throwable): Problem =
    fromLazyThrowable(throwable)

  def fromLazyThrowable(throwable: ⇒ Throwable): Problem =
    new FromThrowable(() ⇒ throwable)

  private class FromString(messageFunction: () ⇒ String) extends Problem {
    private lazy val message = messageFunction()

    def throwable = new ProblemException(message)

    override def equals(o: Any) = o match {
      case o: FromString ⇒ message == o.message
      case _ ⇒ false
    }

    override def hashCode = message.hashCode

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
      Problem(combineMessages(a.toString, b.toString))

    case (a: FromString, b: FromThrowable) ⇒
      Problem.fromLazyThrowable(new ProblemException(a.toString, b.throwable))

    case (a: FromThrowable, b: Problem) ⇒
      Problem.fromLazyThrowable {
        val t = new ProblemException(a.toString, b.throwable)
        t.setStackTrace(a.throwable.getStackTrace)
        t
      }
  }

  private def combineMessages(a: String, b: String) =
    if (a.trim.isEmpty) b
    else if (b.trim.isEmpty) a
    else if (a.matches(".*[.-:] *")) s"${a.trim} $b"
    else s"${a.trim} - $b"
}
