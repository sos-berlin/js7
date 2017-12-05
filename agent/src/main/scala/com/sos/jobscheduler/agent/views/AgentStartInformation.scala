package com.sos.jobscheduler.agent.views

import com.sos.jobscheduler.common.BuildInfo
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
object AgentStartInformation {
  val StartedAt = Instant.now()
  val VersionString = BuildInfo.buildVersion
  val BuildId = BuildInfo.buildId

  def initialize(): Unit = {}
}
