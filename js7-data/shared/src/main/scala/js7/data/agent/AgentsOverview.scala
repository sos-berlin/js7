package js7.data.agent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.item.InventoryItemOverview

/**
  * @author Joacim Zschimmer
  */
final case class AgentsOverview(count: Int) extends InventoryItemOverview

object AgentsOverview extends InventoryItemOverview.Companion[AgentRef]
{
  type Overview = AgentsOverview

  implicit val jsonCodec = deriveCodec[AgentsOverview]

  def itemsToOverview(items: Seq[AgentRef]) = AgentsOverview(items.size)
}
