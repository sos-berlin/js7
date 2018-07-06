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

  override def checked(o: String) =
    if (NamePattern.matcher(o).matches && !o.contains("--"))
      Valid(new UserId(o))
    else
      Invalid(Problem("Not a valid UserId: '$string'"))
}
