package js7.controller.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import java.nio.file.Path
import js7.base.auth.{AgentDirectorForwardPermission, SimpleUser, UpdateItemPermission}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.AsJava.StringAsPath
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.cluster.web.ClusterNodeRouteBindings
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
import js7.data.agent.AgentRefState
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.Stamped
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
  protected val clusterNode: ClusterNode[ControllerState],
  protected val totalRunningSince: Deadline,
  protected val sessionRegister: SessionRegister[SimpleSession],
  protected val eventWatch: FileEventWatch)(
  implicit
    protected val actorSystem: ActorSystem,
    protected val scheduler: Scheduler)
extends AkkaWebServer.BoundRoute
with ServiceProviderRoute
with ControllerRoute
with WebLogDirectives
with ClusterNodeRouteBindings[ControllerState]
{
  protected val controllerState     = clusterNode.currentState
  protected val controllerId        = controllerConfiguration.controllerId
  protected val config              = controllerConfiguration.config
  protected val nodeId              = controllerConfiguration.clusterConf.ownId
  protected val clusterNodeIsBackup = controllerConfiguration.clusterConf.isBackup
  protected val checkedClusterState = controllerState
    .map(_.map(s => Stamped(s.eventId, s.clusterState)))
  protected val currentLogFile      = config.as[Path]("js7.log.file")
  protected val pathToAgentRefState = controllerState.map(_.map(_.keyTo(AgentRefState)))
  protected val routeServiceContext = RouteServiceContext(
    filteredSnapshotRoute, filteredEventRoute, config)
  protected val actorRefFactory     = actorSystem
  protected val gateKeeper = GateKeeper(
    binding,
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply, Seq(
      UpdateItemPermission, AgentDirectorForwardPermission)))

  def boundMessageSuffix = gateKeeper.secureStateString

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta)
  : Task[Checked[command.Response]] =
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
