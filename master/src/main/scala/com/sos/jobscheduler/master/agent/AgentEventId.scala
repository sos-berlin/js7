package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.data.agent.AgentRefId
import com.sos.jobscheduler.data.event.EventId
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AgentEventId(agentRefId: AgentRefId, eventId: EventId)
