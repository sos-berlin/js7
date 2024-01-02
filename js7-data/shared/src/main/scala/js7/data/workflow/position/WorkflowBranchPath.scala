package js7.data.workflow.position

import cats.syntax.show.*
import js7.data.workflow.WorkflowId
import js7.data.workflow.position.BranchPath.syntax.*
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowBranchPath(workflowId: WorkflowId, branchPath: BranchPath):

  def /(nr: InstructionNr): WorkflowPosition =
    WorkflowPosition(workflowId, branchPath % nr)

  override def toString = s"${workflowId.toSimpleString}${branchPath.show}"


object WorkflowBranchPath:
  implicit def apply(workflowId: WorkflowId): WorkflowBranchPath =
    new WorkflowBranchPath(workflowId, Nil)
