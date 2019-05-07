package com.sos.jobscheduler.agent.data.views

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.system.JavaInformation

/**
 * @author Joacim Zschimmer
 */
final case class AgentOverview(
  version: String,
  buildId: String,
  startedAt: Timestamp,
  isTerminating: Boolean,
  system: SystemInformation,
  java: JavaInformation)

object AgentOverview
{
  implicit val jsonCodec: CirceCodec[AgentOverview] = deriveCodec[AgentOverview]
}
