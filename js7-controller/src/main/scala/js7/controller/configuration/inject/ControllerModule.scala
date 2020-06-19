package js7.controller.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.inject.{AbstractModule, Provides}
import com.typesafe.config.Config
import javax.inject.Singleton
import js7.base.auth.{Permission, SimpleUser, UpdateRepoPermission}
import js7.base.eventbus.StandardEventBus
import js7.base.time.ScalaTime._
import js7.base.utils.Closer
import js7.base.utils.ScalazStyle._
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.DeadLetterActor
import js7.common.event.{EventIdClock, EventIdGenerator}
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
import js7.controller.configuration.ControllerConfiguration
import js7.controller.configuration.inject.ControllerModule._
import js7.core.system.ThreadPools
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Deadline.now
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class ControllerModule(configuration: ControllerConfiguration) extends AbstractModule
{
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
  def monixScheduler(): Scheduler = {
    val scheduler = ThreadPools.newStandardScheduler(configuration.name, config)
    closer.onClose {
      scheduler.shutdown()
      //scheduler.awaitTermination(xxx, SECONDS, ???)
    }
    scheduler
  }

  @Provides @Singleton
  def actorRefFactory(actorSystem: ActorSystem): ActorRefFactory =
    actorSystem

  @Provides @Singleton
  def actorSystem(closer: Closer, executionContext: ExecutionContext): ActorSystem = {
    val actorSystem = ActorSystem(configuration.name, config = Some(configuration.config),
      defaultExecutionContext = config.getBoolean("js7.akka.use-js7-thread-pool") ? executionContext)
    closer.onClose {
      logger.debug("ActorSystem.terminate ...")
      try {
        val since = now
        actorSystem.terminate() await config.getDuration("js7.akka.shutdown-timeout").toFiniteDuration
        logger.debug(s"ActorSystem terminated (${since.elapsed.pretty})")
      }
      catch {
        case NonFatal(t) => logger.warn(s"ActorSystem.terminate(): $t")
      }
    }
    DeadLetterActor.subscribe(actorSystem)
    actorSystem
  }

  @Provides @Singleton
  def testEventBus(): StandardEventBus[Any] =
    new StandardEventBus[Any]

  @Provides @Singleton
  def config(): Config =
    configuration.config

  @Provides @Singleton
  def controllerConfiguration(): ControllerConfiguration =
    configuration

  @Provides @Singleton
  def closer(): Closer =
    new Closer
}

object ControllerModule {
  private val logger = Logger(getClass)
  private val stringToPermission = Permission.toStringToPermission(List(
    UpdateRepoPermission))
}
