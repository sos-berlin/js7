package js7.controller.web

import akka.actor.ActorSystem
import com.google.inject.Injector
import com.typesafe.config.Config
import java.nio.file.Path
import javax.inject.{Inject, Singleton}
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs._
import js7.base.convert.AsJava.StringAsPath
import js7.base.problem.Checked
import js7.base.utils.Closer
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.OrderApi
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.controller.web.serviceprovider.RouteServiceContext
import js7.core.command.CommandMeta
import js7.core.item.VersionedItemApi
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.journal.watch.EventWatch
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
final class ControllerWebServer private(
  controllerConfiguration: ControllerConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  itemApi: VersionedItemApi,
  orderApi: OrderApi,
  commandExecutor: ControllerCommandExecutor,
  itemUpdater: ItemUpdater,
  controllerState: Task[Checked[ControllerState]],
  totalRunningSince: Deadline,
  sessionRegister: SessionRegister[SimpleSession],
  eventWatch: EventWatch,
  protected val config: Config,
  injector: Injector,
  implicit protected val actorSystem: ActorSystem,
  protected val scheduler: Scheduler)
extends AkkaWebServer with AkkaWebServer.HasUri
{
  protected val bindings = controllerConfiguration.webServerBindings

  protected def newRoute(binding: WebServerBinding, whenTerminating: Future[Deadline]) =
    new AkkaWebServer.BoundRoute with CompleteRoute {
      protected def whenShuttingDown        = whenTerminating
      protected val controllerConfiguration = ControllerWebServer.this.controllerConfiguration
      protected val controllerId            = controllerConfiguration.controllerId
      protected val nodeId                  = controllerConfiguration.clusterConf.ownId
      protected val injector                = ControllerWebServer.this.injector
      protected val actorSystem             = ControllerWebServer.this.actorSystem
      protected def actorRefFactory         = ControllerWebServer.this.actorSystem
      protected implicit val scheduler      = ControllerWebServer.this.scheduler
      protected val config                  = ControllerWebServer.this.config
      protected val gateKeeper              = new GateKeeper(binding.scheme, gateKeeperConfiguration,
        isLoopback = binding.address.getAddress.isLoopbackAddress)
      protected val sessionRegister     = ControllerWebServer.this.sessionRegister
      protected val eventWatch          = ControllerWebServer.this.eventWatch
      protected val pathToAgentRefState = controllerState.map(_.map(_.pathToAgentRefState))
      protected val itemApi = ControllerWebServer.this.itemApi
      protected val orderApi = ControllerWebServer.this.orderApi
      protected val itemUpdater = ControllerWebServer.this.itemUpdater

      protected def executeCommand(command: ControllerCommand, meta: CommandMeta) = commandExecutor.executeCommand(command, meta)
      protected def checkedClusterState = ControllerWebServer.this.controllerState.map(_.map(_.clusterState))
      protected def clusterNodeIsBackup = controllerConfiguration.clusterConf.isBackup
      protected def controllerState = ControllerWebServer.this.controllerState
      protected def totalRunningSince = ControllerWebServer.this.totalRunningSince
      protected val currentLogFile = config.as[Path]("js7.log.file")
      protected val routeServiceContext = RouteServiceContext(filteredSnapshotRoute, filteredEventRoute)

      def webServerRoute = completeRoute

      override def boundMessageSuffix = gateKeeper.secureStateString
    }
}

object ControllerWebServer
{
  @Singleton
  final class Factory @Inject private(
    controllerConfiguration: ControllerConfiguration,
    gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    sessionRegister: SessionRegister[SimpleSession],
    config: Config,
    injector: Injector,
    actorSystem: ActorSystem,
    scheduler: Scheduler,
    closer: Closer)
  {
    def apply(itemApi: VersionedItemApi, orderApi: OrderApi,
      commandExecutor: ControllerCommandExecutor,
      repoUpdater: ItemUpdater,
      controllerState: Task[Checked[ControllerState]],
      totalRunningSince: Deadline,
      eventWatch: EventWatch)
    : ControllerWebServer =
      new ControllerWebServer(
        controllerConfiguration, gateKeeperConfiguration,
        itemApi, orderApi, commandExecutor, repoUpdater,
        controllerState, totalRunningSince,
        sessionRegister, eventWatch, config, injector,
        actorSystem, scheduler)
      .closeWithCloser(closer)
  }
}
