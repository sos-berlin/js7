package com.sos.jobscheduler.base.problem

import com.sos.jobscheduler.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class ProblemCode private(string: String) extends GenericString

object ProblemCode extends GenericString.NonEmpty[ProblemCode]
{
  val empty = ProblemCode.unchecked("")

  protected def unchecked(string: String) = new ProblemCode(string)
}
