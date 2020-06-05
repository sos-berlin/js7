package js7.master.data.agent

import js7.data.agent.AgentRefPath
import js7.data.event.{Event, EventId}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AgentEventIdEvent(agentEventId: EventId) extends Event {
  type Key = AgentRefPath

  override def toString = s"AgentEventIdEvent(${EventId.toString(agentEventId)})"
}
