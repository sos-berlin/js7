package com.sos.jobscheduler.master.data.agent

import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.EventId
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AgentEventId(agentRefPath: AgentRefPath, eventId: EventId)
