package js7.base.problem

import js7.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class ProblemCode(string: String) extends GenericString

object ProblemCode extends GenericString.NonEmpty[ProblemCode]
{
  val empty = ProblemCode.unchecked("")

  protected def unchecked(string: String) = new ProblemCode(string)
}
