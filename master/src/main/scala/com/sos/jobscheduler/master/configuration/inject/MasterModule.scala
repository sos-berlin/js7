package com.sos.jobscheduler.master.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.base.auth.{SimpleUser, UpdateRepoPermission}
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.akkautils.DeadLetterActor
import com.sos.jobscheduler.common.event.{EventIdClock, EventWatch}
import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.{Closer, Logger}
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.system.ThreadPools
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs._
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule._
import com.sos.jobscheduler.master.data.MasterSnapshots.SnapshotJsonCodec
import com.typesafe.config.Config
import javax.inject.Singleton
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class MasterModule(configuration: MasterConfiguration) extends AbstractModule
{
  @Provides @Singleton
  def eventWatch(p: JournalEventWatch[Event]): EventWatch[Event] =
    p

  @Provides @Singleton
  def journalEventReader(journalMeta: JournalMeta[Event])(implicit s: Scheduler, config: Config, closer: Closer): JournalEventWatch[Event] =
    new JournalEventWatch[Event](journalMeta, config)
      .closeWithCloser

  @Provides @Singleton
  def journalMeta(): JournalMeta[Event] =
    JournalMeta(SnapshotJsonCodec, MasterJournalKeyedEventJsonCodec, masterConfiguration.stateDirectory resolve "master")

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
  def monixScheduler(): Scheduler =
    ThreadPools.newStandardScheduler(configuration.name, config)

  @Provides @Singleton
  def actorRefFactory(actorSystem: ActorSystem): ActorRefFactory =
    actorSystem

  @Provides @Singleton
  def actorSystem(implicit closer: Closer, executionContext: ExecutionContext): ActorSystem = {
    val actorSystem = ActorSystem(configuration.name, config = Some(configuration.config),
      defaultExecutionContext = config.getBoolean("jobscheduler.akka.use-jobscheduler-thread-pool") ? executionContext)
    closer.onClose {
      logger.debug("ActorSystem.terminate ...")
      try {
        actorSystem.terminate() await config.getDuration("jobscheduler.akka.shutdown-timeout").toFiniteDuration
        logger.debug("ActorSystem terminated")
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
  private val simplePermssions = List(UpdateRepoPermission)
  private val stringToPermission = simplePermssions toKeyedMap (_.name)
}
