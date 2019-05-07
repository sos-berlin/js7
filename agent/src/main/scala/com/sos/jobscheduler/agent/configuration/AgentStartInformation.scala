package com.sos.jobscheduler.agent.configuration

import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.BuildInfo

/**
 * @author Joacim Zschimmer
 */
object AgentStartInformation {
  val StartedAt = now
  val PrettyVersion = BuildInfo.prettyVersion
  val BuildId = BuildInfo.buildId

  def initialize(): Unit = {}
}
