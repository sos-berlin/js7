package js7.agent.scheduler

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.controller.ControllerId

final case class RegisteredController(
  controllerId: ControllerId,
  agentRefPath: AgentRefPath,
  agentRunId: AgentRunId)

object RegisteredController
{
  implicit val jsonCodec = deriveCodec[RegisteredController]
}
