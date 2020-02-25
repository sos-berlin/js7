package com.sos.jobscheduler.master.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.base.auth.{Permission, SimpleUser, UpdateRepoPermission}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.akkautils.DeadLetterActor
import com.sos.jobscheduler.common.event.{EventIdClock, EventIdGenerator}
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.{Closer, Logger}
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.core.system.ThreadPools
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule._
import com.typesafe.config.Config
import javax.inject.Singleton
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Deadline.now
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class MasterModule(configuration: MasterConfiguration) extends AbstractModule
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
      defaultExecutionContext = config.getBoolean("jobscheduler.akka.use-jobscheduler-thread-pool") ? executionContext)
    closer.onClose {
      logger.debug("ActorSystem.terminate ...")
      try {
        val since = now
        actorSystem.terminate() await config.getDuration("jobscheduler.akka.shutdown-timeout").toFiniteDuration
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
  def config(): Config =
    configuration.config

  @Provides @Singleton
  def masterConfiguration(): MasterConfiguration =
    configuration

  @Provides @Singleton
  def closer(): Closer =
    new Closer
}

object MasterModule {
  private val logger = Logger(getClass)
  private val stringToPermission = Permission.toStringToPermission(List(
    UpdateRepoPermission))
}
