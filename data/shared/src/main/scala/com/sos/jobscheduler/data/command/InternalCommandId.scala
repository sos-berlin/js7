package com.sos.jobscheduler.data.command

import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Problem
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
final case class InternalCommandId private(number: Long) extends GenericString
{
  def string = number.toString

  override def toString = s"#$number"
}

object InternalCommandId extends GenericString.Checked_[InternalCommandId]
{
  def unchecked(o: String) = InternalCommandId(o.toLong)

  override def checked(o: String) =
    Try(o.toLong) match {
      case Failure(t) => Left(Problem.pure(t))
      case Success(n) => Right(new InternalCommandId(n))
    }

  def newGenerator(): Iterator[InternalCommandId] =
    Iterator.iterate(1L) { _ + 1 } map InternalCommandId.apply
}
