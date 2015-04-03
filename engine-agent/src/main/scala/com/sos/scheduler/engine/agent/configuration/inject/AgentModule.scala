package com.sos.scheduler.engine.agent.configuration.inject

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(agentConfiguration: AgentConfiguration) extends ScalaAbstractModule {

  private val closer = Closer.create()

  protected def configure() = {
    bindInstance[Closer](closer)
    bindInstance[AgentConfiguration](agentConfiguration)
    provideSingleton[ActorSystem] { newActorSystem("JobScheduler-Agent")(closer) }
  }
}
