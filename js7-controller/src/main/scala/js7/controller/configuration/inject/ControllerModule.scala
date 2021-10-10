package js7.controller.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.inject.{AbstractModule, Provides}
import com.typesafe.config.Config
import javax.inject.Singleton
import js7.base.auth.{Permission, SimpleUser, UpdateItemPermission}
import js7.base.eventbus.StandardEventBus
import js7.base.log.Logger
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters._
import js7.base.time.AlarmClock
import js7.base.utils.Closer
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.{Akkas, DeadLetterActor}
import js7.common.system.ThreadPools
import js7.controller.configuration.ControllerConfiguration
import js7.controller.configuration.inject.ControllerModule._
import js7.journal.{EventIdClock, EventIdGenerator}
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class ControllerModule(
  configuration: ControllerConfiguration,
  commonScheduler: Option[Scheduler] = None)
extends AbstractModule
{
  import configuration.config

  @Provides @Singleton
  def eventIdGenerator(eventIdClock: EventIdClock): EventIdGenerator =
    new EventIdGenerator(eventIdClock)

  @Provides @Singleton
  def eventIdClock(): EventIdClock =
    EventIdClock.Default

  @Provides @Singleton
  def sessionRegister(actorSystem: ActorSystem, config: Config)(implicit s: Scheduler): SessionRegister[SimpleSession] =
    SessionRegister.start[SimpleSession](actorSystem, SimpleSession.apply, config)

  @Provides @Singleton
  def gateKeeperConfiguration(config: Config): GateKeeper.Configuration[SimpleUser] =
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply, stringToPermission)

  @Provides @Singleton
  def executionContext(scheduler: Scheduler): ExecutionContext =
    scheduler

  @Provides @Singleton
  def scheduler(closer: Closer): Scheduler =
    commonScheduler getOrElse
      ThreadPools.newStandardScheduler(configuration.name, config, closer)

  @Provides @Singleton
  def actorRefFactory(actorSystem: ActorSystem): ActorRefFactory =
    actorSystem

  @Provides @Singleton
  def actorSystem(closer: Closer, executionContext: ExecutionContext): ActorSystem = {
    import configuration.name
    logger.debug(s"new ActorSystem('$name')")
    val actorSystem = ActorSystem(
      name,
      config = Some(config),
      Some(getClass.getClassLoader),
      defaultExecutionContext = config.getBoolean("js7.akka.use-js7-thread-pool") ? executionContext)
    closer.onClose {
      Akkas.terminateAndWait(actorSystem, config.getDuration("js7.akka.shutdown-timeout").toFiniteDuration)
    }
    DeadLetterActor.subscribe(actorSystem)
    actorSystem
  }

  @Provides @Singleton
  def testEventBus(): StandardEventBus[Any] =
    new StandardEventBus[Any]

  @Provides @Singleton
  def provideConfig(): Config =
    config

  @Provides @Singleton
  def controllerConfiguration(): ControllerConfiguration =
    configuration

  @Provides @Singleton
  def alarmClock(config: Config)(implicit s: Scheduler): AlarmClock =
    AlarmClock(Some(config
      .getDuration("js7.time.clock-setting-check-interval")
      .toFiniteDuration))

  @Provides @Singleton
  def closer(): Closer =
    new Closer
}

object ControllerModule {
  private val logger = Logger(getClass)
  private val stringToPermission = Permission.toStringToPermission(List(
    UpdateItemPermission))
}
