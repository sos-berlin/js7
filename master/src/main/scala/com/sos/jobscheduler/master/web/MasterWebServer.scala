package com.sos.jobscheduler.master.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.{LoginSession, SessionRegister}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.journal.EventReaderProvider
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.master.OrderApi
import com.sos.jobscheduler.master.command.{CommandExecutor, CommandMeta}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.web.MasterWebServer._
import com.typesafe.config.Config
import javax.inject.{Inject, Singleton}
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServer private(
  masterConfiguration: MasterConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  timerService: TimerService,
  injector: Injector,
  fileBasedApi: FileBasedApi,
  orderApi: OrderApi.WithCommands,
  commandExecutor: CommandExecutor)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val executionContext: ExecutionContext)
extends AkkaWebServer with AkkaWebServer.HasUri {

  protected def uriPathPrefix = ""
  protected val bindings = masterConfiguration.webServerBindings

  protected def newRoute(binding: WebServerBinding): Route =
    new CompleteRoute {
      protected val masterId            = masterConfiguration.masterId
      protected val injector            = MasterWebServer.this.injector
      protected val actorSystem         = injector.instance[ActorSystem]
      protected implicit def actorRefFactory = MasterWebServer.this.actorSystem
      protected val config              = injector.instance[Config]
      protected val gateKeeper          = new GateKeeper(gateKeeperConfiguration, timerService, isLoopback = binding.address.getAddress.isLoopbackAddress)
      protected val sessionRegister     = injector.instance[SessionRegister[LoginSession.Simple]]
      protected val eventReader         = injector.instance[EventReaderProvider[Event]]
      protected val scheduler           = injector.instance[Scheduler]
      protected val fileBasedApi = MasterWebServer.this.fileBasedApi
      protected val orderApi = MasterWebServer.this.orderApi
      protected def orderCount = orderApi.orderCount
      protected def executeCommand(command: MasterCommand, meta: CommandMeta) = commandExecutor.executeCommand(command, meta)

      logger.info(gateKeeper.boundMessage(binding.address, binding.scheme))
    }
    .completeRoute
}

object MasterWebServer {
  private val logger = Logger(getClass)

  @Singleton
  final class Factory @Inject private(
    masterConfiguration: MasterConfiguration,
    gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    timerService: TimerService,
    injector: Injector)
    (implicit
      protected val actorSystem: ActorSystem,
      protected val executionContext: ExecutionContext)
  {
    def apply(fileBasedApi: FileBasedApi, orderApi: OrderApi.WithCommands, commandExecutor: CommandExecutor): MasterWebServer =
      new MasterWebServer(masterConfiguration, gateKeeperConfiguration, timerService, injector,
        fileBasedApi, orderApi, commandExecutor)
  }
}
