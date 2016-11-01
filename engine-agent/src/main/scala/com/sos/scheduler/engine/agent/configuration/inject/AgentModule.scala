package com.sos.scheduler.engine.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Injector, Provides}
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import com.sos.scheduler.engine.agent.task.{StandardAgentTaskFactory, TaskHandler}
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.{EncodedPasswordValidator, UserAndPassword}
import com.sos.scheduler.engine.common.configutils.Configs._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.taskserver.moduleapi.ModuleFactoryRegister
import com.sos.scheduler.engine.taskserver.modules.StandardModuleFactories
import com.sos.scheduler.engine.taskserver.modules.shell.ShellModule
import com.typesafe.config.Config
import javax.inject.Singleton
import scala.collection.immutable
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(originalAgentConfiguration: AgentConfiguration)
extends AbstractModule {

  protected def configure() = {}

  @Provides  // Lazy, not (eager) @Singleton. Only HTTPS needs this - and requires passwords.conf.
  def passwordValidator(conf: AgentConfiguration): UserAndPassword ⇒ Boolean =
    EncodedPasswordValidator.fromSubConfig(conf.authUsersConfig)

  @Provides @Singleton
  def extraWebServices(agentConfiguration: AgentConfiguration, injector: Injector): immutable.Seq[ExternalWebService] =
    agentConfiguration.externalWebServiceClasses map { o ⇒ injector.getInstance(o) }

  @Provides @Singleton
  def actorRefFactory(o: ActorSystem): ActorRefFactory = o

  @Provides @Singleton
  def taskHandlerView(o: TaskHandler): TaskHandlerView = o

  @Provides @Singleton
  def moduleFactoryRegister(): ModuleFactoryRegister = {
    val moduleFactories =
      if (StandardAgentTaskFactory.runInProcess) StandardModuleFactories  // For in-process debugging
      else List(ShellModule)  // Other modules get its own process via TaskServerMain
    new ModuleFactoryRegister(moduleFactories)
  }

  @Provides @Singleton
  def timerService(actorSystem: ActorSystem, closer: Closer): TimerService =
    TimerService()(actorSystem.dispatcher) closeWithCloser closer

  @Provides @Singleton
  def actorSystem(closer: Closer, config: Config): ActorSystem =
    newActorSystem("Agent", config)(closer)

  @Provides @Singleton
  def executionContext(actorSystem: ActorSystem): ExecutionContext = actorSystem.dispatcher

  @Provides @Singleton
  def agentConfiguration(): AgentConfiguration = originalAgentConfiguration.finishAndProvideFiles

  @Provides @Singleton
  def config(agentConfiguration: AgentConfiguration): Config = agentConfiguration.config

  @Provides @Singleton
  def closer(): Closer = Closer.create()  // Do not use concurrently !!!
}
