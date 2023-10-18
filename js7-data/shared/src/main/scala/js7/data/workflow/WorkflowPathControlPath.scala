package js7.data.workflow

import js7.base.annotation.javaApi
import js7.base.problem.Checked
import js7.data.item.{InventoryItemPath, UnsignedSimpleItemPath}

final case class WorkflowPathControlPath(workflowPath: WorkflowPath)
extends UnsignedSimpleItemPath, InventoryItemPath.AttachableToAgent:
  
  def string = workflowPath.string
  def companion = WorkflowPathControlPath


object WorkflowPathControlPath extends UnsignedSimpleItemPath.Companion[WorkflowPathControlPath]:
  protected def unchecked(string: String) = new WorkflowPathControlPath(WorkflowPath(string))

  override def checked(string: String): Checked[WorkflowPathControlPath] =
    for
      workflowPath <- WorkflowPath.checked(string)
      path <- super.checked(workflowPath.string)
    yield path

  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(validWorkflowControlPath: String): WorkflowPathControlPath =
    apply(validWorkflowControlPath)
