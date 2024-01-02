package js7.data.workflow.position

import js7.data.workflow.instructions.Fork

// Not used
private object ForkBranchId:
  private val NamePattern = """fork\+(.+)""".r

  def unapply(branchId: BranchId): Option[Fork.Branch.Id] =
    branchId match
      case BranchId.Named(NamePattern(forkBranchId)) =>
        Some(Fork.Branch.Id(forkBranchId))

      case _ => None
