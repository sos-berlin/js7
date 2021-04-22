package js7.agent.scheduler

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId

final case class RegisteredController(
  controllerId: ControllerId,
  agentId: AgentPath,
  agentRunId: AgentRunId)

object RegisteredController
{
  implicit val jsonCodec = deriveCodec[RegisteredController]
}
