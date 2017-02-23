package com.sos.scheduler.engine.master

import com.sos.scheduler.engine.data.engine2.agent.AgentPath
import com.sos.scheduler.engine.data.event.EventId
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class AgentEventId(agentPath: AgentPath, eventId: EventId)

object AgentEventId {
  implicit val jsonFormat = jsonFormat2(apply)
}
