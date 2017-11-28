package com.sos.jobscheduler.agent.views

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs.InstantJsonCodec
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.system.JavaInformation
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
final case class AgentOverview(
  version: String,
  startedAt: Instant,
  isTerminating: Boolean,
  system: SystemInformation,
  java: JavaInformation)

object AgentOverview {
  intelliJuseImport(InstantJsonCodec)

  implicit val JsonCodec: CirceCodec[AgentOverview] = deriveCirceCodec[AgentOverview]
}
