package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.data.agent.AgentId
import com.sos.jobscheduler.data.event.{Event, EventId}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AgentEventIdEvent(agentEventId: EventId) extends Event {
  type Key = AgentId

  override def toString = s"AgentEventIdEvent(${EventId.toString(agentEventId)})"
}
