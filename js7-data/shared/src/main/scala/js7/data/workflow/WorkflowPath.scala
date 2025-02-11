package js7.data.workflow

import js7.base.annotation.javaApi
import js7.data.item.{InventoryItemPath, SourceType, VersionedItemPath}
import js7.data.workflow.WorkflowControlPath.mayThrow

final case class WorkflowPath private(string: String)
extends VersionedItemPath, InventoryItemPath.AttachableToAgent:
  def companion: VersionedItemPath.Companion[? <: VersionedItemPath] = WorkflowPath


object WorkflowPath extends VersionedItemPath.Companion[WorkflowPath]:
  override val sourceTypeToFilenameExtension: Map[SourceType, String] = Map(
    SourceType.Json -> ".workflow.json",
    SourceType.Txt -> ".workflow.txt")

  protected def unchecked(string: String) = new WorkflowPath(string)

  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(validWorkflowPath: String): WorkflowPath =
    mayThrow(validWorkflowPath)
