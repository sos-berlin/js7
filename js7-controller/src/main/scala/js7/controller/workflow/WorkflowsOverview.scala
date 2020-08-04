package js7.controller.workflow

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.item.InventoryItemOverview
import js7.data.workflow.Workflow

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowsOverview(count: Int) extends InventoryItemOverview

object WorkflowsOverview extends InventoryItemOverview.Companion[Workflow] {
  type Overview = WorkflowsOverview

  implicit val jsonCodec = deriveCodec[WorkflowsOverview]

  def itemsToOverview(items: Seq[Workflow]) = WorkflowsOverview(items.size)
}
