package js7.controller.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.nio.file.Path
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.AsJava.StringAsPath
import js7.base.io.JavaResource
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.cluster.web.ClusterNodeRouteBindings
import js7.common.pekkohttp.PekkoHttpServerUtils.{passIf, pathSegment}
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer.RouteBinding
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.common.web.serviceprovider.{RouteServiceContext, ServiceProviderRoute}
import js7.controller.OrderApi
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.controller.web.ControllerRoute.*
import js7.controller.web.controller.TestRoute
import js7.controller.web.controller.api.ApiRoute
import js7.core.command.CommandMeta
import js7.data.agent.AgentRefState
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.Stamped
import js7.journal.watch.FileEventWatch
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.HttpEntity
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.util.ByteString
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
final class ControllerRoute(
  routeBinding: RouteBinding,
  protected val controllerConfiguration: ControllerConfiguration,
  protected val orderApi: OrderApi,
  commandExecutor: ControllerCommandExecutor,
  protected val itemUpdater: ItemUpdater,
  protected val clusterNode: ClusterNode[ControllerState],
  protected val totalRunningSince: Deadline,
  protected val sessionRegister: SessionRegister[SimpleSession],
  protected val eventWatch: FileEventWatch,
  gateKeeperConf: GateKeeper.Configuration[SimpleUser])(
  implicit
    protected val actorSystem: ActorSystem,
    protected val ioRuntime: IORuntime)
extends
  ServiceProviderRoute,
  WebLogDirectives,
  ClusterNodeRouteBindings[ControllerState],
  ApiRoute,
  TestRoute:

  import routeBinding.webServerBinding

  protected def whenShuttingDown    = routeBinding.whenStopRequested
  protected val controllerState     = clusterNode.currentState
  protected val controllerId        = controllerConfiguration.controllerId
  protected val config              = controllerConfiguration.config
  protected def commonConf          = controllerConfiguration
  protected val nodeId              = controllerConfiguration.clusterConf.ownId
  protected val clusterNodeIsBackup = controllerConfiguration.clusterConf.isBackup
  protected val checkedClusterState = controllerState
    .map(_.map(s => Stamped(s.eventId, s.clusterState)))
  protected val currentLogFile      = config.as[Path]("js7.log.file")
  protected val pathToAgentRefState = controllerState.map(_.map(_.keyTo(AgentRefState)))
  override protected val routeServiceContext = RouteServiceContext(
    filteredSnapshotRoute, filteredEventRoute, config)
  protected val actorRefFactory     = actorSystem
  protected val gateKeeper = GateKeeper(webServerBinding, gateKeeperConf)

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta)
  : IO[Checked[command.Response]] =
    commandExecutor.executeCommand(command, meta)

  logger.debug(s"new ControllerRoute($webServerBinding #${routeBinding.revision})")

  def webServerRoute: Route =
    mainRoute:
      pathPrefix(Segment):
        case "controller" => controllerRoute
        case "grafana" =>
          (path("dashboard") & get):
            // A service for the developer
            seal:
              if GrafanaDashbboardJson.exists then
                complete:
                  HttpEntity.Strict(`application/json`, GrafanaDashbboardJson.readAs[ByteString])
              else
                complete(NotFound, "No Grafana dashboard here")
        case _ => reject
      ~ serviceProviderRoute

  private val controllerRoute: Route =
    pathSegment("api"):
      seal:
        apiRoute
    ~
      pathSegment("TEST"):
        passIf(config.getBoolean("js7.web.server.test")):
          testRoute


object ControllerRoute:
  private val logger = Logger[this.type]

  private val GrafanaDashbboardJson = JavaResource("js7/common/pekkohttp/web/prometheus/grafana-dashboard.json")
