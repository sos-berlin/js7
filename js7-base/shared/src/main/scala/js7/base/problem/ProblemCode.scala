package js7.base.problem

import js7.base.annotation.javaApi
import js7.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class ProblemCode(string: String) extends GenericString


object ProblemCode extends GenericString.NonEmpty[ProblemCode]:
  val empty: ProblemCode = unchecked("")

  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(string: String): ProblemCode =
    apply(string)

  protected def unchecked(string: String) =
    new ProblemCode(string)
