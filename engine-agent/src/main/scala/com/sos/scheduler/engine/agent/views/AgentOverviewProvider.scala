package com.sos.scheduler.engine.agent.views

import com.google.inject.Provider
import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverviewProvider._
import com.sos.scheduler.engine.common.maven.MavenProperties
import com.sos.scheduler.engine.common.utils.JavaResource
import java.time.Instant
import javax.inject.{Inject, Singleton}

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class AgentOverviewProvider @Inject private(processHandlerView: ProcessHandlerView) extends Provider[AgentOverview] {

  def get() = AgentOverview(
    version = AgentMavenProperties.buildVersion,
    startedAt = Instant.now(),
    currentProcessCount = processHandlerView.currentProcessCount,
    totalProcessCount = processHandlerView.totalProcessCount,
    isTerminating = processHandlerView.isTerminating,
    java = AgentOverview.JavaInformation.Singleton)
}

object AgentOverviewProvider {
  private val AgentMavenProperties = new MavenProperties(JavaResource("com/sos/scheduler/engine/agent/maven.properties"))
}
