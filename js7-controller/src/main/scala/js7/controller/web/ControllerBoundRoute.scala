package js7.controller.web

import com.google.inject.Injector
import java.nio.file.Path
import js7.base.auth.{SimpleUser, UpdateItemPermission}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.AsJava.StringAsPath
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.CSRF.forbidCSRF
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.OrderApi
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.controller.web.controller.ControllerRoute
import js7.controller.web.serviceprovider.{RouteServiceContext, ServiceProviderRoute}
import js7.core.command.CommandMeta
import js7.data.agent.AgentRefState
import js7.data.cluster.{ClusterCommand, ClusterWatchingCommand}
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.Stamped
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
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
  clusterNode: ClusterNode[ControllerState],
  protected val totalRunningSince: Deadline,
  protected val sessionRegister: SessionRegister[SimpleSession],
  protected val eventWatch: FileEventWatch,
  protected val injector: Injector)(
  implicit
    protected val actorSystem: ActorSystem,
    protected val scheduler: Scheduler)
extends PekkoWebServer.BoundRoute
with ServiceProviderRoute
with ControllerRoute
with WebLogDirectives
{
  protected val controllerId        = controllerConfiguration.controllerId
  protected val config              = controllerConfiguration.config
  protected val nodeId              = controllerConfiguration.clusterConf.ownId
  protected val clusterNodeIsBackup = controllerConfiguration.clusterConf.isBackup
  protected val checkedClusterState = controllerState
    .map(_.map(s => Stamped(s.eventId, s.clusterState)))
  protected val currentLogFile      = config.as[Path]("js7.log.file")
  protected val pathToAgentRefState = controllerState.map(_.map(_.keyTo(AgentRefState)))
  protected val routeServiceContext = RouteServiceContext(filteredSnapshotRoute, filteredEventRoute)
  protected val actorRefFactory     = actorSystem
  protected val clusterWatchRequestStream = clusterNode.clusterWatchRequestStream
  protected val gateKeeper = GateKeeper(
    binding,
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply, Seq(
      UpdateItemPermission)))

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta)
  : Task[Checked[command.Response]] =
    commandExecutor.executeCommand(command, meta)

  protected def executeClusterCommand(command: ClusterCommand) =
    clusterNode.executeCommand(command)

  protected def executeClusterWatchingCommand(cmd: ClusterWatchingCommand) =
    clusterNode.executeClusterWatchingCommand(cmd)

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
