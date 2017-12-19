package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.data.agent.AgentPath

/**
  * @author Joacim Zschimmer
  */
final case class AgentJobPath(agentPath: AgentPath, jobPath: JobPath)

object AgentJobPath {
  implicit val jsonCodec = deriveCirceCodec[AgentJobPath]
}
