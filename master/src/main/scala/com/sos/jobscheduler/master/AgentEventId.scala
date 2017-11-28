package com.sos.jobscheduler.master

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.EventId
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AgentEventId(agentPath: AgentPath, eventId: EventId)
