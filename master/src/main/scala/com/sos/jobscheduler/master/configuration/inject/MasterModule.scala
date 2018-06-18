package com.sos.jobscheduler.master.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{LoginSession, SessionRegister}
import com.sos.jobscheduler.common.akkautils.DeadLetterActor
import com.sos.jobscheduler.common.event.EventIdClock
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.journal.{EventReaderProvider, JournalEventReaderProvider}
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.master.MasterOrderKeeper
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule._
import com.typesafe.config.Config
import javax.inject.Singleton
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class MasterModule(configuration: MasterConfiguration) extends AbstractModule {

  @Provides @Singleton
  def eventReaderProvider(p: JournalEventReaderProvider[Event]): EventReaderProvider[Event] =
    p

  @Provides @Singleton
  def journalEventReaderProvider()(implicit s: Scheduler, ts: TimerService, config: Config): JournalEventReaderProvider[Event] =
    new JournalEventReaderProvider[Event](
      MasterOrderKeeper.journalMeta,
      configuration.journalFile)

  @Provides @Singleton
  def eventIdClock(): EventIdClock =
    EventIdClock.Default

  @Provides @Singleton
  def sessionRegister(actorSystem: ActorSystem, conf: MasterConfiguration)(implicit s: Scheduler): SessionRegister[LoginSession.Simple] =
    SessionRegister.start[LoginSession.Simple](actorSystem, LoginSession.Simple.apply,
      sessionTimeout = conf.config.getDuration("jobscheduler.auth.session.timeout").toFiniteDuration,
      conf.akkaAskTimeout)

  @Provides @Singleton
  def gateKeeperConfiguration(config: Config): GateKeeper.Configuration[SimpleUser] =
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply)

  @Provides @Singleton
  def executionContext(scheduler: Scheduler): ExecutionContext =
    scheduler

  @Provides @Singleton
  def monixScheduler(): Scheduler =
    //if (sys.runtime.availableProcessors > 1 &&
    //    !sys.props.contains("scala.concurrent.context.minThreads") &&
    //    !sys.props.contains("scala.concurrent.context.numThreads"))
      Scheduler.global
    //else
    //  Scheduler(Executors.newScheduledThreadPool(2))

  @Provides @Singleton
  def actorRefFactory(actorSystem: ActorSystem): ActorRefFactory =
    actorSystem

  @Provides @Singleton
  def actorSystem(implicit closer: Closer, timerService: TimerService/*closed after ActorSystem*/): ActorSystem = {
    val actorSystem = ActorSystem("Master", configuration.config)
    closer.onClose {
      logger.debug("ActorSystem.terminate ...")
      try {
        actorSystem.terminate() await 3.s
        logger.debug("ActorSystem terminated")
      }
      catch {
        case NonFatal(t) ⇒ logger.warn(s"ActorSystem.terminate(): $t")
      }
    }
    DeadLetterActor.subscribe(actorSystem)
    actorSystem
  }

  @Provides @Singleton
  def timerService(closer: Closer): TimerService =
    TimerService() closeWithCloser closer

  @Provides @Singleton
  def config(): Config =
    configuration.config

  @Provides @Singleton
  def masterConfiguration(): MasterConfiguration =
    configuration

  @Provides @Singleton
  def closer(): Closer =
    Closer.create()   // Not thread-safe !!!
}

object MasterModule {
  private val logger = Logger(getClass)
}
