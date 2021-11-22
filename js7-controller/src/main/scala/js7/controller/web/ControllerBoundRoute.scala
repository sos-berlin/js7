package js7.controller.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import java.nio.file.Path
import js7.base.auth.{SimpleUser, UpdateItemPermission}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.AsJava.StringAsPath
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.OrderApi
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.controller.web.controller.ControllerRoute
import js7.controller.web.serviceprovider.{RouteServiceContext, ServiceProviderRoute}
import js7.core.command.CommandMeta
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
final class ControllerBoundRoute(
  binding: WebServerBinding,
  protected val whenShuttingDown: Future[Deadline],
  protected val controllerConfiguration: ControllerConfiguration,
  protected val orderApi: OrderApi,
  commandExecutor: ControllerCommandExecutor,
  protected val itemUpdater: ItemUpdater,
  protected val controllerState: Task[Checked[ControllerState]],
  protected val totalRunningSince: Deadline,
  protected val sessionRegister: SessionRegister[SimpleSession],
  protected val eventWatch: FileEventWatch,
  protected val injector: Injector)(
  implicit
    protected val actorSystem: ActorSystem,
    protected val scheduler: Scheduler)
extends AkkaWebServer.BoundRoute
with ServiceProviderRoute
with ControllerRoute
with WebLogDirectives
{
  override protected type Session = SimpleSession

  protected val controllerId        = controllerConfiguration.controllerId
  protected val nodeId              = controllerConfiguration.clusterConf.ownId
  protected val config              = controllerConfiguration.config
  protected val clusterNodeIsBackup = controllerConfiguration.clusterConf.isBackup
  protected val currentLogFile      = config.as[Path]("js7.log.file")
  protected val pathToAgentRefState = controllerState.map(_.map(_.pathToAgentRefState))
  protected val checkedClusterState = controllerState.map(_.map(_.clusterState))
  protected val routeServiceContext = RouteServiceContext(filteredSnapshotRoute, filteredEventRoute)
  protected val actorRefFactory     = actorSystem

  protected val gateKeeper = GateKeeper(
    binding,
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply, Seq(
      UpdateItemPermission)))

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta) =
    commandExecutor.executeCommand(command, meta)

  val webServerRoute: Route =
    (decodeRequest & encodeResponse) {  // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      webLog {
        forbidCSRF {
          route
        }
      }
    }

  private lazy val route =
    pathSegment("controller") {
      controllerRoute
    } ~
    serviceProviderRoute  // External service provider's routes, for example Controller's own experimental GUI
}
