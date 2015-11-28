package com.sos.scheduler.engine.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{Injector, Provides}
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import com.sos.scheduler.engine.agent.task.TaskHandler
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import javax.inject.Singleton
import scala.collection.immutable

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

  @Provides @Singleton
  private def extraWebServices(injector: Injector): immutable.Seq[ExternalWebService] =
    agentConfiguration.externalWebServiceClasses map { o ⇒ injector.getInstance(o) }

  @Provides @Singleton
  private def actorRefFactory(o: ActorSystem): ActorRefFactory = o

  @Provides
  private def taskHandlerView(o: TaskHandler): TaskHandlerView = o
}
