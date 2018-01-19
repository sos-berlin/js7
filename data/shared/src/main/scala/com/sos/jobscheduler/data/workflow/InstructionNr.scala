package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.generic.GenericInt
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class InstructionNr(number: Int) extends GenericInt {
  require(number >= InstructionNr.FirstInt, s"Negative Index? $number")

  def +(n: Int) = copy(number + n)

  override def toString = s"#$number"
}

object InstructionNr extends GenericInt.Companion[InstructionNr] {
  private val FirstInt = 0
  val First = InstructionNr(FirstInt)

  implicit def fromInt(nr: Int) = new InstructionNr(nr)
}
