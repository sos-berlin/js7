package com.sos.jobscheduler.data.workflow.position

import com.sos.jobscheduler.data.workflow.WorkflowId
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowBranchPath(workflowId: WorkflowId, branchPath: BranchPath)
{
  def /(nr: InstructionNr): WorkflowPosition =
    WorkflowPosition(workflowId, branchPath % nr)
}

object WorkflowBranchPath
{
  implicit def apply(workflowId: WorkflowId): WorkflowBranchPath =
    new WorkflowBranchPath(workflowId, Nil)
}
