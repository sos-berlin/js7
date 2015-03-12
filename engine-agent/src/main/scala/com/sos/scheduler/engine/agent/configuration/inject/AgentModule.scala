package com.sos.scheduler.engine.agent.configuration.inject

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.google.inject.Provides
import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.agent.task.{RemoteTask, RemoteTaskFactory}
import com.sos.scheduler.engine.agent.{AgentCommandExecutor, AgentConfiguration, CommandExecutor}
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.data.agent.RemoteTaskId
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import javax.inject.Singleton

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(agentConfiguration: AgentConfiguration) extends ScalaAbstractModule {

  private val closer = Closer.create()

  protected def configure() = {
    bindInstance[Closer](closer)
    bindInstance[AgentConfiguration](agentConfiguration)
    provide[ActorSystem] { newActorSystem("JobScheduler-Agent")(closer) }
    bindClass[CommandExecutor] to classOf[AgentCommandExecutor]
  }

  @Provides @Singleton
  private def newRemoteTask: TaskStartArguments ⇒ RemoteTask = RemoteTaskFactory.apply

  @Provides @Singleton
  private def newRemoteTaskId: () ⇒ RemoteTaskId = RemoteTaskId.newGenerator().next
}

object AgentModule {
  private val ConfigurationResourcePath = "com/sos/scheduler/engine/agent/configuration/akka.conf"
}
