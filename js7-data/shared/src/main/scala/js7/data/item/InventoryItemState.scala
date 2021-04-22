package js7.data.item

import js7.data.agent.AgentPath

trait InventoryItemState
{
  def item: InventoryItem

  def agentPathToAttachedState: Map[AgentPath, ItemAttachedState.NotDetached]
}
