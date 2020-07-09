package js7.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.inject.{AbstractModule, Provides}
import com.typesafe.config.Config
import javax.inject.Singleton
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.web.AgentWebServer
import js7.base.auth.SimpleUser
import js7.base.utils.Closer
import js7.base.utils.Closer.syntax._
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.event.EventIdGenerator
import js7.common.scalautil.IOExecutor
import js7.common.system.ThreadPools
import js7.core.cluster.ClusterWatchRegister
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(originalAgentConfiguration: AgentConfiguration)
extends AbstractModule
{
  @Provides @Singleton
  def eventIdGenerator(): EventIdGenerator =
    new EventIdGenerator()

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
    newAgentActorSystem(conf.name, config, executionContext)(closer)

  @Provides @Singleton
  def ioExecutor(closer: Closer, conf: AgentConfiguration, config: Config): IOExecutor = {
    val threadPool = IOExecutor.newThreadPoolExecutor(config, name = conf.name)
    closer.onClose { threadPool.shutdown() }
    new IOExecutor(threadPool)
  }

  @Provides @Singleton
  def executionContext(scheduler: Scheduler): ExecutionContext =
    scheduler

  @Provides @Singleton
  def monixScheduler(configuration: AgentConfiguration, closer: Closer): Scheduler = {
    val scheduler = ThreadPools.newStandardScheduler(configuration.name, configuration.config)
    closer.onClose {
      scheduler.shutdown()
      //scheduler.awaitTermination(xxx, SECONDS, ???)
    }
    scheduler
  }

  @Provides @Singleton
  def agentConfiguration(): AgentConfiguration = originalAgentConfiguration.finishAndProvideFiles

  @Provides @Singleton
  def config(agentConfiguration: AgentConfiguration): Config = agentConfiguration.config

  @Provides @Singleton
  def closer(): Closer = new Closer

  @Provides @Singleton
  def provideAgentWebServer(conf: AgentConfiguration, gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    sessionRegister: SessionRegister[SimpleSession],
    clusterWatchRegister: ClusterWatchRegister,
    config: Config,
    actorSystem: ActorSystem, scheduler: Scheduler, closer: Closer): AgentWebServer =
      new AgentWebServer(conf, gateKeeperConfiguration, sessionRegister, clusterWatchRegister,
        config, actorSystem, scheduler
      ).closeWithCloser(closer)
}
