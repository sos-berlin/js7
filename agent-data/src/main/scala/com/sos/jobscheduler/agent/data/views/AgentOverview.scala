package com.sos.jobscheduler.agent.data.views

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.instant.StringInstantJsonCodec
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.system.JavaInformation
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
final case class AgentOverview(
  version: String,
  buildId: String,
  startedAt: Instant,
  isTerminating: Boolean,
  system: SystemInformation,
  java: JavaInformation)

object AgentOverview
{
  intelliJuseImport(StringInstantJsonCodec)

  implicit val jsonCodec: CirceCodec[AgentOverview] = deriveCodec[AgentOverview]
}
