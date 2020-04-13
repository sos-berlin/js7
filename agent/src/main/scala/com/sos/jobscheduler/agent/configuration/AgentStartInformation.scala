package com.sos.jobscheduler.agent.configuration

import com.sos.jobscheduler.base.BuildInfo
import com.sos.jobscheduler.base.time.Timestamp

/**
 * @author Joacim Zschimmer
 */
object AgentStartInformation {
  val StartedAt = Timestamp.now
  val PrettyVersion = BuildInfo.prettyVersion
  val BuildId = BuildInfo.buildId

  def initialize(): Unit = {}
}
