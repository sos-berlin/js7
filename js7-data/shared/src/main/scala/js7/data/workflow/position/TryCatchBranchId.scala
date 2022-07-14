package js7.data.workflow.position

import js7.base.utils.ScalaUtils.syntax.*
import js7.data.workflow.position.BranchId.{Catch_, Try_}

private[workflow] object TryCatchBranchId
{
  private val NamePattern = """(?:try|catch)\+([0-9]+)""".r

  def unapply(branchId: BranchId): Option[Int] = branchId match {
    case Try_ | Catch_ => Some(0)
    case BranchId.Named(NamePattern(nr)) =>
      val i = Integer.parseInt(nr)
      (i >= 0) ? i
    case _ => None
  }
}

object TryBranchId
{
  private val NamePattern = """try\+([0-9]+)""".r

  def unapply(branchId: BranchId): Option[Int] = branchId match {
    case Try_ => Some(0)
    case BranchId.Named(NamePattern(nr)) =>
      val i = Integer.parseInt(nr)
      (i >= 0) ? i
    case _ => None
  }
}

private[workflow] object CatchBranchId
{
  private val NamePattern = """catch\+([0-9]+)""".r

  def unapply(branchId: BranchId): Option[Int] = branchId match {
    case Catch_ => Some(0)
    case BranchId.Named(NamePattern(nr)) =>
      val i = Integer.parseInt(nr)
      (i >= 0) ? i
    case _ => None
  }
}
