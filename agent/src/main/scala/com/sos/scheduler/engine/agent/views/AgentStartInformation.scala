package com.sos.scheduler.engine.agent.views

import com.sos.scheduler.engine.common.BuildInfo
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
object AgentStartInformation {
  val StartedAt = Instant.now()
  val VersionString = BuildInfo.buildVersion

  def initialize(): Unit = {}
}
