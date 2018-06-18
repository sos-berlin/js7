package com.sos.jobscheduler.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Injector, Provides}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.task.StandardAgentTaskFactory
import com.sos.jobscheduler.agent.web.AgentWebServer
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{LoginSession, SessionRegister}
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.taskserver.data.TaskServerMainTerminated
import com.sos.jobscheduler.taskserver.moduleapi.ModuleFactoryRegister
import com.sos.jobscheduler.taskserver.modules.javamodule.{JavaScriptEngineModule, StandardJavaModule}
import com.sos.jobscheduler.taskserver.modules.shell.ShellModule
import com.typesafe.config.Config
import javax.inject.Singleton
import monix.execution.Scheduler
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(originalAgentConfiguration: AgentConfiguration)
extends AbstractModule {

  @Provides @Singleton
  def sessionRegister(actorSystem: ActorSystem, conf: AgentConfiguration)(implicit s: Scheduler): SessionRegister[LoginSession.Simple] =
    SessionRegister.start[LoginSession.Simple](actorSystem, LoginSession.Simple.apply,
      sessionTimeout = conf.config.getDuration("jobscheduler.auth.session.timeout").toFiniteDuration,
      akkaAskTimeout = conf.akkaAskTimeout)

  @Provides @Singleton
  def gateKeeperConfiguration(config: Config): GateKeeper.Configuration[SimpleUser] =
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply)

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
  def executionContext(scheduler: Scheduler): ExecutionContext =
    scheduler

  @Provides @Singleton
  def monixScheduler(): Scheduler =
    Scheduler.global

  @Provides @Singleton
  def agentConfiguration(): AgentConfiguration = originalAgentConfiguration.finishAndProvideFiles

  @Provides @Singleton
  def config(agentConfiguration: AgentConfiguration): Config = agentConfiguration.config

  @Provides @Singleton
  def closer(): Closer = Closer.create()  // Do not use concurrently !!!

  @Provides @Singleton
  def provideAgentWebServer(conf: AgentConfiguration, gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    timerService: TimerService,
    closer: Closer, injector: Injector, actorSystem: ActorSystem, executionContext: ExecutionContext): AgentWebServer =
      new AgentWebServer(conf, gateKeeperConfiguration, timerService, closer, injector)(actorSystem, executionContext)
        .closeWithCloser(closer)
}
