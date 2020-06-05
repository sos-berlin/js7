package js7.master.data.agent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
final case class AgentSnapshot(agentRefPath: AgentRefPath, agentRunId: Option[AgentRunId], eventId: EventId)

object AgentSnapshot
{
  implicit val jsonCodec = deriveCodec[AgentSnapshot]
}
