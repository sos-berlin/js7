package com.sos.scheduler.engine.agent.views

import com.google.inject.Provider
import com.sos.scheduler.engine.agent.task.TaskHandler
import com.sos.scheduler.engine.common.system.SystemInformations.systemInformation
import com.sos.scheduler.engine.data.system.JavaInformation
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
