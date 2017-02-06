package com.sos.scheduler.engine.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Injector, Provides}
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import com.sos.scheduler.engine.agent.task.{StandardAgentTaskFactory, TaskHandler}
import com.sos.scheduler.engine.agent.web.common.{ExternalWebService, LoginSession}
import com.sos.scheduler.engine.common.auth.EncodedPasswordValidator
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.sprayutils.web.auth.{CSRF, GateKeeper}
import com.sos.scheduler.engine.common.sprayutils.web.session.SessionRegister
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.taskserver.data.TaskServerMainTerminated
import com.sos.scheduler.engine.taskserver.moduleapi.ModuleFactoryRegister
import com.sos.scheduler.engine.taskserver.modules.javamodule.{JavaScriptEngineModule, StandardJavaModule}
import com.sos.scheduler.engine.taskserver.modules.shell.ShellModule
import com.typesafe.config.Config
import javax.inject.Singleton
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(originalAgentConfiguration: AgentConfiguration)
extends AbstractModule {

  protected def configure() = {}

  @Provides @Singleton
  def sessionRegister(): SessionRegister[LoginSession] =
    new SessionRegister[LoginSession]

  @Provides @Singleton
  def provideCsrfConfiguration(config: Config): CSRF.Configuration =
    CSRF.Configuration.fromSubConfig(config.getConfig("jobscheduler.agent.webserver.csrf"))

  @Provides @Singleton
  def provideGateKeeperConfiguration(config: Config): GateKeeper.Configuration =
    GateKeeper.Configuration
      .fromSubConfig(config.getConfig("jobscheduler.agent.webserver.auth"))
      .copy(providePasswordValidator = () ⇒ EncodedPasswordValidator.fromSubConfig(config.getConfig("jobscheduler.agent.auth.users")))

  @Provides @Singleton
  def extraWebServices(agentConfiguration: AgentConfiguration, injector: Injector): immutable.Seq[ExternalWebService] =
    agentConfiguration.externalWebServiceClasses map { o ⇒ injector.getInstance(o) }

  @Provides @Singleton
  def actorRefFactory(o: ActorSystem): ActorRefFactory = o

  @Provides @Singleton
  def taskHandlerView(o: TaskHandler): TaskHandlerView = o

  @Provides @Singleton
  def moduleFactoryRegister(shellModuleFactory: ShellModule.Factory): ModuleFactoryRegister = {
    val forInProcessDebugging = if (StandardAgentTaskFactory.runInProcess)
      List(  // For in-process debugging
        StandardJavaModule,
        JavaScriptEngineModule,
        shellModuleFactory)
      else
        Nil  // Other modules get its own process via TaskServerMain
    new ModuleFactoryRegister(shellModuleFactory :: forInProcessDebugging)
  }

  /** If task server runs in an own process, the Future of its termination. */
  @Provides @Singleton
  def TerminatedFutureOption: Option[Future[TaskServerMainTerminated.type]] = None

  @Provides @Singleton
  def timerService(actorSystem: ActorSystem, closer: Closer): TimerService =
    TimerService() closeWithCloser closer

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
