package com.sos.jobscheduler.master.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.{Closer, Logger}
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.master.command.MasterCommandExecutor
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.web.MasterWebServer._
import com.sos.jobscheduler.master.{MasterState, OrderApi}
import com.typesafe.config.Config
import javax.inject.{Inject, Singleton}
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServer private(
  masterConfiguration: MasterConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  fileBasedApi: FileBasedApi,
  orderApi: OrderApi.WithCommands,
  commandExecutor: MasterCommandExecutor,
  masterState: Task[MasterState],
  sessionRegister: SessionRegister[SimpleSession],
  eventWatch: EventWatch[Event],
  protected val config: Config,
  injector: Injector,
  implicit protected val actorSystem: ActorSystem,
  protected val scheduler: Scheduler)
extends AkkaWebServer with AkkaWebServer.HasUri {

  protected def uriPathPrefix = ""
  protected val bindings = masterConfiguration.webServerBindings

  protected def newRoute(binding: WebServerBinding): Route =
    new CompleteRoute {
      protected val masterConfiguration = MasterWebServer.this.masterConfiguration
      protected val masterId            = masterConfiguration.masterId
      protected val injector            = MasterWebServer.this.injector
      protected val actorSystem         = MasterWebServer.this.actorSystem
      protected implicit def actorRefFactory = MasterWebServer.this.actorSystem
      protected implicit val scheduler  = MasterWebServer.this.scheduler
      protected val config              = MasterWebServer.this.config
      protected val gateKeeper          = new GateKeeper(gateKeeperConfiguration,
        isLoopback = binding.address.getAddress.isLoopbackAddress,
        mutual = binding.mutual)
      protected val sessionRegister     = MasterWebServer.this.sessionRegister
      protected val eventWatch          = MasterWebServer.this.eventWatch
      protected val fileBasedApi = MasterWebServer.this.fileBasedApi
      protected val orderApi = MasterWebServer.this.orderApi
      protected def orderCount = orderApi.orderCount
      protected def executeCommand(command: MasterCommand, meta: CommandMeta) = commandExecutor.executeCommand(command, meta)
      protected def masterState = MasterWebServer.this.masterState

      logger.info(gateKeeper.boundMessage(binding))
    }
    .completeRoute
}

object MasterWebServer {
  private val logger = Logger(getClass)

  @Singleton
  final class Factory @Inject private(
    masterConfiguration: MasterConfiguration,
    gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    sessionRegister: SessionRegister[SimpleSession],
    config: Config,
    injector: Injector,
    actorSystem: ActorSystem,
    scheduler: Scheduler,
    closer: Closer)
  {
    def apply(fileBasedApi: FileBasedApi, orderApi: OrderApi.WithCommands,
      commandExecutor: MasterCommandExecutor, masterState: Task[MasterState],
      eventWatch: EventWatch[Event])
    : MasterWebServer =
      new MasterWebServer(masterConfiguration, gateKeeperConfiguration,
        fileBasedApi, orderApi, commandExecutor, masterState,
        sessionRegister, eventWatch, config, injector,
        actorSystem, scheduler)
      .closeWithCloser(closer)
  }
}
