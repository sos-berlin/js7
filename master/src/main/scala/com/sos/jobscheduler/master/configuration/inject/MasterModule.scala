package com.sos.jobscheduler.master.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.common.akkahttp.web.auth.{CSRF, GateKeeper}
import com.sos.jobscheduler.common.akkautils.DeadLetterActor
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.ActorEventCollector
import com.sos.jobscheduler.master.agent.AgentEventIdEvent
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
  def eventCollector(factory: ActorEventCollector.Factory): EventCollector =
    factory.apply {
      case _: AgentEventIdEvent ⇒ false
      case _ ⇒ true
    }

  @Provides @Singleton
  def eventCollectorConfiguration(config: Config): EventCollector.Configuration =
    EventCollector.Configuration.fromSubConfig(config.getConfig("jobscheduler.master.event"))

  @Provides @Singleton
  def csrfConfiguration(config: Config): CSRF.Configuration =
    CSRF.Configuration.fromSubConfig(config.getConfig("jobscheduler.master.webserver.csrf"))

  @Provides @Singleton
  def gateKeeperConfiguration: GateKeeper.Configuration =
    GateKeeper.Configuration.fromSubConfig(configuration.config.getConfig("jobscheduler.master.webserver.auth"))

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
