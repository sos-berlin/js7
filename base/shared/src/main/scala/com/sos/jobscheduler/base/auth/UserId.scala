package com.sos.jobscheduler.base.auth

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
final case class UserId private(string: String) extends GenericString

object UserId extends GenericString.Companion[UserId] {
  private val NamePattern = """^([\p{L}0-9_][\p{L}0-9_.-]*)$""".r.pattern
  val Anonymous = UserId("Anonymous")

  def apply(o: String) = UserId.checked(o).orThrow

  override def checked(string: String) =
    if (NamePattern.matcher(string).matches && !string.contains("--"))
      Valid(new UserId(string))
    else
      Invalid(Problem(s"Not a valid UserId: '$string'"))
}
