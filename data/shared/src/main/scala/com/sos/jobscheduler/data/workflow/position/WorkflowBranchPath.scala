package com.sos.jobscheduler.data.workflow.position

import com.sos.jobscheduler.data.workflow.WorkflowId
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowBranchPath(workflowId: WorkflowId, parents: BranchPath)
{
  def /(nr: InstructionNr): WorkflowPosition =
    WorkflowPosition(workflowId, parents / nr)
}

object WorkflowBranchPath
{
  implicit def apply(workflowId: WorkflowId) = new WorkflowBranchPath(workflowId, BranchPath.Empty)
}
