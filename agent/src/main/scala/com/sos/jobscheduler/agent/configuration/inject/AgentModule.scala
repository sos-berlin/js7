package com.sos.jobscheduler.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Injector, Provides}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.task.StandardAgentTaskFactory
import com.sos.jobscheduler.agent.web.AgentWebServer
import com.sos.jobscheduler.agent.web.common.{ExternalWebService, LoginSession}
import com.sos.jobscheduler.common.akkahttp.web.auth.{CSRF, GateKeeper}
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegister
import com.sos.jobscheduler.common.auth.EncodedToHashedPassword
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.taskserver.data.TaskServerMainTerminated
import com.sos.jobscheduler.taskserver.moduleapi.ModuleFactoryRegister
import com.sos.jobscheduler.taskserver.modules.javamodule.{JavaScriptEngineModule, StandardJavaModule}
import com.sos.jobscheduler.taskserver.modules.shell.ShellModule
import com.typesafe.config.Config
import javax.inject.Singleton
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(originalAgentConfiguration: AgentConfiguration)
extends AbstractModule {

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
      .copy(userIdToHashedPassword = () ⇒ EncodedToHashedPassword.fromSubConfig(config.getConfig("jobscheduler.agent.auth.users")))

  @Provides @Singleton
  def extraWebServices(agentConfiguration: AgentConfiguration, injector: Injector): immutable.Seq[ExternalWebService] =
    agentConfiguration.externalWebServiceClasses map { o ⇒ injector.getInstance(o) }

  @Provides @Singleton
  def actorRefFactory(o: ActorSystem): ActorRefFactory = o

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
  def actorSystem(closer: Closer, conf: AgentConfiguration, config: Config, timerService: TimerService/*closed after ActorSystem*/): ActorSystem =
    newActorSystem(conf.name, config)(closer)

  @Provides @Singleton
  def timerService(closer: Closer): TimerService =
    TimerService() closeWithCloser closer

  @Provides @Singleton
  def executionContext(actorSystem: ActorSystem): ExecutionContext = actorSystem.dispatcher

  @Provides @Singleton
  def agentConfiguration(): AgentConfiguration = originalAgentConfiguration.finishAndProvideFiles

  @Provides @Singleton
  def config(agentConfiguration: AgentConfiguration): Config = agentConfiguration.config

  @Provides @Singleton
  def closer(): Closer = Closer.create()  // Do not use concurrently !!!

  @Provides @Singleton
  def provideAgentWebServer(conf: AgentConfiguration, gateKeeperConfiguration: GateKeeper.Configuration, csrf: CSRF,
    timerService: TimerService,
    closer: Closer, injector: Injector, actorSystem: ActorSystem, executionContext: ExecutionContext): AgentWebServer =
      new AgentWebServer(conf, gateKeeperConfiguration, csrf, timerService, closer, injector)(actorSystem, executionContext)
        .closeWithCloser(closer)
}
