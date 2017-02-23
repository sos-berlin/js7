package com.sos.jobscheduler.agent.views

import com.google.inject.Provider
import com.sos.jobscheduler.agent.task.TaskHandler
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.data.system.JavaInformation
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentOverviewProvider @Inject private(taskHandler: TaskHandler) extends Provider[AgentOverview] {

  def get() = {
    val taskView = taskHandler.overview
    AgentOverview(
      version = AgentStartInformation.VersionString,
      startedAt = AgentStartInformation.StartedAt,
      currentTaskCount = taskView.currentTaskCount,
      totalTaskCount = taskView.totalTaskCount,
      isTerminating = taskHandler.isTerminating,
      system = systemInformation(),
      java = JavaInformation())
  }
}
