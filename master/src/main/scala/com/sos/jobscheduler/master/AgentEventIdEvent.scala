package com.sos.jobscheduler.master

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{Event, EventId}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AgentEventIdEvent(agentEventId: EventId) extends Event {
  type Key = AgentPath

  override def toString = s"AgentEventIdEvent(${EventId.toString(agentEventId)})"
}
