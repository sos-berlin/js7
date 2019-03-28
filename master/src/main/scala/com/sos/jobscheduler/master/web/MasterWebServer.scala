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
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Logger
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
  injector: Injector,
  fileBasedApi: FileBasedApi,
  orderApi: OrderApi.WithCommands,
  commandExecutor: MasterCommandExecutor,
  masterState: Task[MasterState],
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
      protected val actorSystem         = injector.instance[ActorSystem]
      protected implicit def actorRefFactory = MasterWebServer.this.actorSystem
      protected implicit val scheduler  = MasterWebServer.this.scheduler
      protected val config              = injector.instance[Config]
      protected val gateKeeper          = new GateKeeper(gateKeeperConfiguration,
        isLoopback = binding.address.getAddress.isLoopbackAddress,
        mutual = binding.mutual)
      protected val sessionRegister     = injector.instance[SessionRegister[SimpleSession]]
      protected val eventWatch          = injector.instance[EventWatch[Event]]
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
    injector: Injector,
    actorSystem: ActorSystem,
    scheduler: Scheduler)
  {
    def apply(fileBasedApi: FileBasedApi, orderApi: OrderApi.WithCommands,
      commandExecutor: MasterCommandExecutor, masterState: Task[MasterState])
    : MasterWebServer =
      new MasterWebServer(masterConfiguration, gateKeeperConfiguration, injector,
        fileBasedApi, orderApi, commandExecutor, masterState,
        actorSystem, scheduler)
  }
}
