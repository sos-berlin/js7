package js7.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.inject.{AbstractModule, Provides}
import com.typesafe.config.Config
import javax.inject.Singleton
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.web.common.AgentSession
import js7.base.auth.SimpleUser
import js7.base.eventbus.StandardEventBus
import js7.base.log.CorrelId
import js7.base.thread.IOExecutor
import js7.base.thread.ThreadPoolsBase.newBlockingExecutor
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.{AlarmClock, WallClock}
import js7.base.utils.Closer
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.SessionRegister
import js7.common.system.ThreadPools
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.journal.{EventIdClock, EventIdGenerator}
import js7.launcher.configuration.JobLauncherConf
import monix.execution.Scheduler
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
  def sessionRegister(actorSystem: ActorSystem, config: Config)(implicit s: Scheduler): SessionRegister[AgentSession] =
    SessionRegister.start[AgentSession](actorSystem, AgentSession.apply, config)

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
    val threadPool = newBlockingExecutor(config, name = conf.name + " I/O")
    closer.onClose { threadPool.shutdown() }
    new IOExecutor(threadPool)
  }

  @Provides @Singleton
  def executionContext(scheduler: Scheduler): ExecutionContext =
    scheduler

  /** Do not override this, override alarmClock! */
  @Provides @Singleton
  def wallClock(clock: AlarmClock): WallClock =
    clock

  @Provides @Singleton
  def alarmClock(config: Config, scheduler: Scheduler): AlarmClock =
    AlarmClock(
      Some(config.getDuration("js7.time.clock-setting-check-interval").toFiniteDuration))(
      scheduler)

  @Provides @Singleton
  def scheduler(configuration: AgentConfiguration, closer: Closer): Scheduler =
    commonScheduler.map(CorrelId.enableScheduler(_)) getOrElse
      ThreadPools.newStandardScheduler(configuration.name, configuration.config, closer)

  @Provides @Singleton
  def jobLauncherConf(conf: AgentConfiguration, iox: IOExecutor, clock: AlarmClock, closer: Closer)
  : JobLauncherConf = {
    val blockingJobScheduler: Scheduler = {
      // For BlockingInternalJob (thread-blocking Java jobs)
      val scheduler = newUnlimitedScheduler("JS7 blocking job")
      closer.onClose(scheduler.shutdown())
      CorrelId.enableScheduler(scheduler)
    }
    conf.toJobLauncherConf(iox, blockingJobScheduler, clock).orThrow
  }

  @Provides @Singleton
  def agentConfiguration(): AgentConfiguration = originalAgentConfiguration.finishAndProvideFiles

  @Provides @Singleton
  def config(agentConfiguration: AgentConfiguration): Config = agentConfiguration.config

  @Provides @Singleton
  def testEventBus(): StandardEventBus[Any] =
    new StandardEventBus[Any]

  @Provides @Singleton
  def closer(): Closer = new Closer
}
