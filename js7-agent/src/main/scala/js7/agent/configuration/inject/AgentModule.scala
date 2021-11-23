package js7.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.inject.{AbstractModule, Provides}
import com.typesafe.config.Config
import javax.inject.Singleton
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.web.AgentWebServer
import js7.base.auth.SimpleUser
import js7.base.thread.IOExecutor
import js7.base.thread.ThreadPoolsBase.newUnlimitedThreadPool
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.{AlarmClock, WallClock}
import js7.base.utils.Closer
import js7.base.utils.Closer.syntax._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.system.ThreadPools
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.core.cluster.ClusterWatchRegister
import js7.executor.configuration.JobExecutorConf
import js7.journal.{EventIdClock, EventIdGenerator}
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(
  originalAgentConfiguration: AgentConfiguration,
  commonScheduler: Option[Scheduler] = None)
extends AbstractModule
{
  @Provides @Singleton
  def eventIdGenerator(eventIdClock: EventIdClock): EventIdGenerator =
    new EventIdGenerator(eventIdClock)

  @Provides @Singleton
  def eventIdClock(clock: WallClock): EventIdClock =
    EventIdClock(clock)

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
    val threadPool = newUnlimitedThreadPool(config, name = conf.name + " I/O")
    closer.onClose { threadPool.shutdown() }
    new IOExecutor(threadPool)
  }

  @Provides @Singleton
  def executionContext(scheduler: Scheduler): ExecutionContext =
    scheduler

  /** Do not override this, override alarmClock! */
  @Provides @Singleton
  def wallClock(clock: AlarmClock)(implicit s: Scheduler): WallClock =
    clock

  @Provides @Singleton
  def alarmClock(config: Config, scheduler: Scheduler): AlarmClock =
    AlarmClock(
      Some(config.getDuration("js7.time.clock-setting-check-interval").toFiniteDuration))(
      scheduler)

  @Provides @Singleton
  def scheduler(configuration: AgentConfiguration, closer: Closer): Scheduler =
    commonScheduler getOrElse
      ThreadPools.newStandardScheduler(configuration.name, configuration.config, closer)

  @Provides @Singleton
  def jobExecutorConf(conf: AgentConfiguration, iox: IOExecutor, clock: AlarmClock, closer: Closer)
  : JobExecutorConf = {
    val blockingJobScheduler: SchedulerService = {
      // For BlockingInternalJob (thread-blocking Java jobs)
      val scheduler = newUnlimitedScheduler("JS7 blocking job")
      closer.onClose(scheduler.shutdown())
      scheduler
    }
    conf.toExecutorConf(iox, blockingJobScheduler, clock).orThrow
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
