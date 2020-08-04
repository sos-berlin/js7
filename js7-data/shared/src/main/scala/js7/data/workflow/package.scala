package js7.data

import js7.data.item.ItemId

/**
  * @author Joacim Zschimmer
  */
package object workflow
{
  type WorkflowId = ItemId[WorkflowPath]
  val WorkflowId = new ItemId.Companion[WorkflowPath] {}
}
