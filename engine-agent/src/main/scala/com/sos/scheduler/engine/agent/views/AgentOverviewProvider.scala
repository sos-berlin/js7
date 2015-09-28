package com.sos.scheduler.engine.agent.views

import com.google.inject.Provider
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentOverviewProvider @Inject private(taskHandlerViewProvider: Provider[TaskHandlerView]) extends Provider[AgentOverview] {

  def get() = {
    val taskView = taskHandlerViewProvider.get()
    AgentOverview(
      version = AgentStartInformation.VersionString,
      startedAt = AgentStartInformation.StartedAt,
      currentTaskCount = taskView.currentTaskCount,
      totalTaskCount = taskView.totalTaskCount,
      isTerminating = taskView.isTerminating,
      system = AgentOverview.SystemInformation(),
      java = AgentOverview.JavaInformation.Singleton)
  }
}
