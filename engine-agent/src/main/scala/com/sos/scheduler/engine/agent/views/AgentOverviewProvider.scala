package com.sos.scheduler.engine.agent.views

import com.google.inject.Provider
import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentOverviewProvider @Inject private(processHandlerView: ProcessHandlerView) extends Provider[AgentOverview] {

  def get() = AgentOverview(
    version = AgentStartInformation.VersionString,
    startedAt = AgentStartInformation.StartedAt,
    currentProcessCount = processHandlerView.currentProcessCount,
    totalProcessCount = processHandlerView.totalProcessCount,
    isTerminating = processHandlerView.isTerminating,
    system = AgentOverview.SystemInformation(),
    java = AgentOverview.JavaInformation.Singleton)
}
