package js7.agent.scheduler

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId

final case class RegisteredController(
  controllerId: ControllerId,
  agentPath: AgentPath,
  agentRunId: AgentRunId)

object RegisteredController:
  implicit val jsonCodec: Codec.AsObject[RegisteredController] = deriveCodec[RegisteredController]
