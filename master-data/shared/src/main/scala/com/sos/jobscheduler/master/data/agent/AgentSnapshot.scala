package com.sos.jobscheduler.master.data.agent

import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.event.EventId
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AgentSnapshot(agentRefPath: AgentRefPath, agentRunId: Option[AgentRunId], eventId: EventId)
