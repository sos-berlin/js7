package com.sos.jobscheduler.base.auth

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Problems.InvalidNameProblem

/**
  * @author Joacim Zschimmer
  */
final case class UserId private(string: String) extends GenericString
{
  def isAnonymous = this == UserId.Anonymous
}

object UserId extends GenericString.Checked_[UserId]
{
  private val NamePattern = """^([\p{L}0-9_][\p{L}0-9_.-]*)$""".r.pattern  // TODO Use NameValidator
  val Anonymous = UserId("Anonymous")

  def unchecked(string: String) = new UserId(string)

  override def checked(string: String) =
    if (NamePattern.matcher(string).matches && !string.contains("--"))
      super.checked(string)
    else
      Invalid(InvalidNameProblem("UserId", string))
}
