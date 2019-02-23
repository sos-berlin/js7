package com.sos.jobscheduler.data.workflow.position

import com.sos.jobscheduler.base.generic.GenericInt
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class InstructionNr(number: Int) extends GenericInt
{
  import InstructionNr._

  require(number >= InstructionNr.FirstInt, s"Negative Index? $number")

  def +(n: Int) = copy(number + n)

  def /(branchId: BranchId) = BranchPath.Segment(this, branchId)

  override def toString = s"$Prefix$number"
}

object InstructionNr extends GenericInt.Companion[InstructionNr]
{
  private val FirstInt = 0
  val First = InstructionNr(FirstInt)
  val Prefix = ":"

  implicit def fromInt(nr: Int): InstructionNr = new InstructionNr(nr)
}
