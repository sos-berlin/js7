package js7.agent.scheduler

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.master.MasterId

final case class RegisteredMaster(
  masterId: MasterId,
  agentRefPath: AgentRefPath,
  agentRunId: AgentRunId)

object RegisteredMaster
{
  implicit val jsonCodec = deriveCodec[RegisteredMaster]
}
