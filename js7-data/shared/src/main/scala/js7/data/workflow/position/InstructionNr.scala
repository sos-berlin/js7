package js7.data.workflow.position

import js7.base.generic.GenericInt
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class InstructionNr(number: Int) extends GenericInt
{
  import InstructionNr.*

  require(number >= InstructionNr.FirstInt, s"Negative Index? $number")

  def increment: InstructionNr =
    copy(number + 1)

  def /(branchId: BranchId) = BranchPath.Segment(this, branchId)

  override def toString = s"$Prefix$number"
}

object InstructionNr extends GenericInt.Companion[InstructionNr]
{
  private val FirstInt = 0
  private lazy val predefined = (0 to 999).map(i => new InstructionNr(i)).toVector
  lazy val First = InstructionNr(FirstInt)
  val Prefix = ":"

  def apply(number: Int): InstructionNr =
    if number >= 0 && number < predefined.size then
      predefined(number)
    else
      new InstructionNr(number)

  implicit def fromInt(nr: Int): InstructionNr =
    apply(nr)
}
