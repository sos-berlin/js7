package js7.data.workflow

import js7.base.annotation.javaApi
import js7.base.problem.Checked
import js7.data.item.{InventoryItemPath, UnsignedItemPath, VersionedControlPath}

final case class WorkflowControlPath(workflowPath: WorkflowPath)
extends UnsignedItemPath
with VersionedControlPath
with InventoryItemPath.AttachableToAgent:

  def path = this
  def string = workflowPath.string
  def companion = WorkflowControlPath


object WorkflowControlPath extends VersionedControlPath.Companion[WorkflowControlPath]:
  protected def unchecked(string: String) = new WorkflowControlPath(WorkflowPath(string))

  override def checked(string: String): Checked[WorkflowControlPath] =
    for
      workflowPath <- WorkflowPath.checked(string)
      path <- super.checked(workflowPath.string)
    yield path

  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(validWorkflowControlPath: String): WorkflowControlPath =
    apply(validWorkflowControlPath)
