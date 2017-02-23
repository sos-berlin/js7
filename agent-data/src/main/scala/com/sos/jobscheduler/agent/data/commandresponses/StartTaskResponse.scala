package com.sos.jobscheduler.agent.data.commandresponses

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.tunnel.data.TunnelToken
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class StartTaskResponse(
  agentTaskId: AgentTaskId,
  tunnelToken: TunnelToken)
extends Response

object StartTaskResponse {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
