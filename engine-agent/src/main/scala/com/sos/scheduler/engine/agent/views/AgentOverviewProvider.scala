package com.sos.scheduler.engine.agent.views

import com.google.inject.Provider
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentOverviewProvider @Inject private(TaskHandlerView: TaskHandlerView) extends Provider[AgentOverview] {

  def get() = AgentOverview(
    version = AgentStartInformation.VersionString,
    startedAt = AgentStartInformation.StartedAt,
    currentTaskCount = TaskHandlerView.currentTaskCount,
    totalTaskCount = TaskHandlerView.totalTaskCount,
    isTerminating = TaskHandlerView.isTerminating,
    system = AgentOverview.SystemInformation(),
    java = AgentOverview.JavaInformation.Singleton)
}
