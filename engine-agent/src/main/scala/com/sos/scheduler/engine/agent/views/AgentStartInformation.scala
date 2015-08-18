package com.sos.scheduler.engine.agent.views

import com.sos.scheduler.engine.common.maven.MavenProperties
import com.sos.scheduler.engine.common.utils.JavaResource
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
object AgentStartInformation {
  private val AgentMavenProperties = new MavenProperties(JavaResource("com/sos/scheduler/engine/agent/maven.properties"))
  val StartedAt = Instant.now()
  val VersionString = AgentMavenProperties.buildVersion

  def initialize() {}
}
