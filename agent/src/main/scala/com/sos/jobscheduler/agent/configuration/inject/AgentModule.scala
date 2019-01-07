package com.sos.jobscheduler.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.inject.{AbstractModule, Injector, Provides}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.web.AgentWebServer
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.{Closer, IOExecutor}
import com.sos.jobscheduler.core.system.ThreadPools
import com.typesafe.config.Config
import javax.inject.Singleton
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(originalAgentConfiguration: AgentConfiguration)
extends AbstractModule {

  @Provides @Singleton
  def sessionRegister(actorSystem: ActorSystem, config: Config)(implicit s: Scheduler): SessionRegister[SimpleSession] =
    SessionRegister.start[SimpleSession](actorSystem, SimpleSession.apply, config)

  @Provides @Singleton
  def gateKeeperConfiguration(config: Config): GateKeeper.Configuration[SimpleUser] =
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply)

  @Provides @Singleton
  def actorRefFactory(o: ActorSystem): ActorRefFactory = o

  @Provides @Singleton
  def actorSystem(closer: Closer, conf: AgentConfiguration, config: Config, executionContext: ExecutionContext): ActorSystem =
    newActorSystem(conf.name, config, executionContext)(closer)

  @Provides @Singleton
  def ioExecutor(closer: Closer, config: Config): IOExecutor = {
    val threadPool = IOExecutor.newThreadPoolExecutor(config)
    closer.onClose { threadPool.shutdown() }
    new IOExecutor(threadPool)
  }

  @Provides @Singleton
  def executionContext(scheduler: Scheduler): ExecutionContext =
    scheduler

  @Provides @Singleton
  def monixScheduler(configuration: AgentConfiguration): Scheduler =
    ThreadPools.newStandardScheduler(configuration.name, configuration.config)

  @Provides @Singleton
  def agentConfiguration(): AgentConfiguration = originalAgentConfiguration.finishAndProvideFiles

  @Provides @Singleton
  def config(agentConfiguration: AgentConfiguration): Config = agentConfiguration.config

  @Provides @Singleton
  def closer(): Closer = new Closer

  @Provides @Singleton
  def provideAgentWebServer(conf: AgentConfiguration, gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    closer: Closer, injector: Injector, actorSystem: ActorSystem, scheduler: Scheduler): AgentWebServer =
      new AgentWebServer(conf, gateKeeperConfiguration, closer, injector, actorSystem, scheduler)
        .closeWithCloser(closer)
}
