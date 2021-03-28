package js7.data

import js7.data.item.VersionedItemId

/**
  * @author Joacim Zschimmer
  */
package object workflow
{
  type WorkflowId = VersionedItemId[WorkflowPath]

  val WorkflowId = WorkflowPath.versionedItemIdCompanion
}
