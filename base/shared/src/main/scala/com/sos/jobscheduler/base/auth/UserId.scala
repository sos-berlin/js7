package com.sos.jobscheduler.base.auth

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{CheckedString, Problem}

/**
  * @author Joacim Zschimmer
  */
final case class UserId(string: String) extends GenericString {
  UserId.checked(string).orThrow
}

object UserId extends GenericString.Companion[UserId] with CheckedString[UserId] {
  private val NamePattern = """^([\p{L}0-9_][\p{L}0-9_.-]*)$""".r.pattern
  val Anonymous = UserId("Anonymous")

  override def checked(o: String) =
    if (NamePattern.matcher(o).matches)
      Valid(UserId(o))
    else
      Invalid(Problem("Not a valid UserId: '$string'"))
}
