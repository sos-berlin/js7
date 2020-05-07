package com.sos.jobscheduler.master.data.agent

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
final case class AgentSnapshot(agentRefPath: AgentRefPath, agentRunId: Option[AgentRunId], eventId: EventId)

object AgentSnapshot
{
  implicit val jsonCodec = deriveCodec[AgentSnapshot]
}
