package com.sos.scheduler.engine.master

import com.sos.scheduler.engine.data.engine2.agent.AgentPath
import com.sos.scheduler.engine.data.event.{Event, EventId}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class AgentEventIdEvent(agentEventId: EventId) extends Event {
  type Key = AgentPath

  override def toString = s"AgentEventIdEvent(${EventId.toString(agentEventId)})"
}

object AgentEventIdEvent {
  implicit val jsonFormt = jsonFormat1(apply)
}
