package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commandresponses.EmptyResponse
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class CloseTask(agentTaskId: AgentTaskId, kill: Boolean)
extends TaskCommand {
  type Response = EmptyResponse.type
}

object CloseTask {
  val SerialTypeName = "CloseTask"
  implicit val MyJsonFormat = jsonFormat2(apply)
}
