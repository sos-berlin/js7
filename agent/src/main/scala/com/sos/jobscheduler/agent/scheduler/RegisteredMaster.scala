package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.data.agent.AgentRunId
import com.sos.jobscheduler.data.master.MasterId

final case class RegisteredMaster(masterId: MasterId, agentRunId: AgentRunId)

object RegisteredMaster
{
  implicit val jsonCodec = deriveCodec[RegisteredMaster]
}
