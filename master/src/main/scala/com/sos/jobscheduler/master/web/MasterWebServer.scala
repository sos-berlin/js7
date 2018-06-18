package com.sos.jobscheduler.master.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.{LoginSession, SessionRegister}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.{Logger, SetOnce}
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.journal.EventReaderProvider
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.master.OrderApi
import com.sos.jobscheduler.master.command.CommandMeta
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.web.MasterWebServer._
import com.typesafe.config.Config
import javax.inject.{Inject, Singleton}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class MasterWebServer @Inject private(
  masterConfiguration: MasterConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  timerService: TimerService,
  injector: Injector)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val executionContext: ExecutionContext)
extends AkkaWebServer with AkkaWebServer.HasUri {

  protected def uriPathPrefix = ""
  protected val bindings = masterConfiguration.webServerBindings
  private val orderApiOnce = new SetOnce[OrderApi.WithCommands]("OrderApi")
  private val fileBasedApiOnce = new SetOnce[FileBasedApi]("FileBasedApi")
  private val executeCommandOnce = new SetOnce[(MasterCommand, CommandMeta) ⇒ Task[Checked[MasterCommand.Response]]]

  def setClients(fileBasedApi: FileBasedApi, orderApi: OrderApi.WithCommands): Unit = {
    fileBasedApiOnce := fileBasedApi
    orderApiOnce := orderApi
  }

  def setExecuteCommand(executeCommand: (MasterCommand, CommandMeta) ⇒ Task[Checked[MasterCommand.Response]]) =
    executeCommandOnce := executeCommand

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
      protected val fileBasedApi = fileBasedApiOnce()
      protected val orderApi = orderApiOnce()
      protected def orderCount = orderApi.orderCount
      protected def executeCommand(command: MasterCommand, meta: CommandMeta) = executeCommandOnce()(command, meta)

      logger.info(gateKeeper.boundMessage(binding.address, binding.scheme))
    }
    .completeRoute
}

object MasterWebServer {
  private val logger = Logger(getClass)
}
