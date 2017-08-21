package com.sos.jobscheduler.agent.views

import com.google.inject.Provider
import com.sos.jobscheduler.agent.scheduler.OrderHandler
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.data.system.JavaInformation
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentOverviewProvider @Inject private(orderHandler: OrderHandler) extends Provider[AgentOverview] {

  def get() = {
    AgentOverview(
      version = AgentStartInformation.VersionString,
      startedAt = AgentStartInformation.StartedAt,
      currentTaskCount = 0,  // TODO
      totalTaskCount = 0,    // TODO
      isTerminating = false, // TODO
      system = systemInformation(),
      java = JavaInformation())
  }
}
