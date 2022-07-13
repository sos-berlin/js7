package js7.data

import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.item.{UnsignedVersionedItemId, VersionId, VersionedItemId}

package object workflow
{
  type WorkflowId = VersionedItemId[WorkflowPath]
  val WorkflowId = WorkflowPath.VersionedItemIdCompanion

  type WorkflowControlId = UnsignedVersionedItemId[WorkflowControlPath]

  object WorkflowControlId extends UnsignedVersionedItemId.Companion[WorkflowControlPath]
  {
    def apply(idString: String): UnsignedVersionedItemId[WorkflowControlPath] =
      checked(idString).orThrow

    def apply(workflowId: WorkflowId): UnsignedVersionedItemId[WorkflowControlPath] =
      new UnsignedVersionedItemId(
        WorkflowControlPath(workflowId.path),
        workflowId.versionId)

    def apply(path: WorkflowControlPath, versionId: VersionId) =
      path ~ versionId

    val pathCompanion = WorkflowControlPath

    // A versioned pathTypeName may not differ from its itemTypePath
    def pathTypeName = itemTypeName

    object syntax {
      implicit final class RichWorkflowControlId(private val workflowControlId: WorkflowControlId)
      extends AnyVal {
        def workflowId = WorkflowId(workflowPath, workflowControlId.versionId)

        def workflowPath = WorkflowPath(workflowControlId.path.string)
      }
    }
  }
}
