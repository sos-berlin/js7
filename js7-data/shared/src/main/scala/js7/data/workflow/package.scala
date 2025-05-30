package js7.data

import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.item.{UnsignedVersionedItemId, VersionId, VersionedControlPath, VersionedItemId}

package object workflow:
  type WorkflowId = VersionedItemId[WorkflowPath]
  val WorkflowId: WorkflowPath.VersionedItemIdCompanion.type = WorkflowPath.VersionedItemIdCompanion

  type WorkflowControlId = UnsignedVersionedItemId[WorkflowControlPath]

  object WorkflowControlId extends UnsignedVersionedItemId.Companion[WorkflowControlPath]:
    def apply(idString: String): UnsignedVersionedItemId[WorkflowControlPath] =
      checked(idString).orThrow

    def apply(workflowId: WorkflowId): UnsignedVersionedItemId[WorkflowControlPath] =
      new UnsignedVersionedItemId(
        WorkflowControlPath(workflowId.path),
        workflowId.versionId)

    def apply(path: WorkflowControlPath, versionId: VersionId)
    : UnsignedVersionedItemId[WorkflowControlPath] =
      path ~ versionId

    val pathCompanion: VersionedControlPath.Companion[WorkflowControlPath] = WorkflowControlPath

    // A versioned pathTypeName may not differ from its itemTypePath
    def pathTypeName: String = itemTypeName

    object syntax:
      implicit final class RichWorkflowControlId(private val workflowControlId: WorkflowControlId)
      extends AnyVal:
        def workflowId: WorkflowId =
          WorkflowId(workflowPath, workflowControlId.versionId)

        def workflowPath: WorkflowPath =
          WorkflowPath(workflowControlId.path.string)
