package com.sos.jobscheduler.master

import com.sos.jobscheduler.data.engine2.agent.AgentPath
import com.sos.jobscheduler.data.event.EventId
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class AgentEventId(agentPath: AgentPath, eventId: EventId)

object AgentEventId {
  implicit val jsonFormat = jsonFormat2(apply)
}
