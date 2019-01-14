package com.sos.jobscheduler.agent.configuration

import com.sos.jobscheduler.common.BuildInfo
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
object AgentStartInformation {
  val StartedAt = Instant.now()
  val PrettyVersion = BuildInfo.prettyVersion
  val BuildId = BuildInfo.buildId

  def initialize(): Unit = {}
}
