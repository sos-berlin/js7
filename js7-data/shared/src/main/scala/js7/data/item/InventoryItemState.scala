package js7.data.item

import js7.data.agent.AgentId

trait InventoryItemState
{
  def item: InventoryItem

  def agentIdToAttachedState: Map[AgentId, ItemAttachedState.NotDetached]
}
