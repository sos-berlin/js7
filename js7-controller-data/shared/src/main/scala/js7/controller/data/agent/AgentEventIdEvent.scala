package js7.controller.data.agent

import io.circe.generic.semiauto.deriveCodec
import js7.data.agent.AgentRefPath
import js7.data.event.{Event, EventId}

/**
  * @author Joacim Zschimmer
  */
final case class AgentEventIdEvent(agentEventId: EventId) extends Event {
  type Key = AgentRefPath

  override def toString = s"AgentEventIdEvent(${EventId.toString(agentEventId)})"
}

object AgentEventIdEvent
{
  implicit val jsonCodec = deriveCodec[AgentEventIdEvent]
}
