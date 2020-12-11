package js7.controller.workflow

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.item.VersionedItemOverview
import js7.data.workflow.Workflow

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowsOverview(count: Int) extends VersionedItemOverview

object WorkflowsOverview extends VersionedItemOverview.Companion[Workflow] {
  type Overview = WorkflowsOverview

  implicit val jsonCodec = deriveCodec[WorkflowsOverview]

  def itemsToOverview(items: Seq[Workflow]) = WorkflowsOverview(items.size)
}
