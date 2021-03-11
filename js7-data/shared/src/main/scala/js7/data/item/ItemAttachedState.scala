package js7.data.item

import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.agent.AgentId

final case class ItemAttachedState(agentIds: Set[AgentId])
{
  def isAttachedTo(agentId: AgentId) =
    agentIds(agentId)
}

object ItemAttachedState
{
  def empty = ItemAttachedState(Set.empty)

  implicit val jsonCodec: CirceObjectCodec[ItemAttachedState] =
    deriveCodec[ItemAttachedState]
}
