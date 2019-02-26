package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{Event, EventId}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AgentEventIdEvent(agentEventPath: EventId) extends Event {
  type Key = AgentRefPath

  override def toString = s"AgentEventIdEvent(${EventId.toString(agentEventPath)})"
}
