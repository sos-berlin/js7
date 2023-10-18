package js7.controller.web

import java.nio.file.Path
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.AsJava.StringAsPath
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.cluster.web.ClusterNodeRouteBindings
import js7.common.pekkohttp.PekkoHttpServerUtils.{passIf, pathSegment}
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer.RouteBinding
import js7.common.pekkohttp.web.auth.CSRF.forbidCSRF
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.OrderApi
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.controller.web.ControllerRoute.*
import js7.controller.web.controller.TestRoute
import js7.controller.web.controller.api.ApiRoute
import js7.controller.web.serviceprovider.{RouteServiceContext, ServiceProviderRoute}
import js7.core.command.CommandMeta
import js7.data.agent.AgentRefState
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.Stamped
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
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
    protected val scheduler: Scheduler)
extends ServiceProviderRoute
with WebLogDirectives
with ClusterNodeRouteBindings[ControllerState]
with ApiRoute
with TestRoute:
  import routeBinding.webServerBinding

  protected def whenShuttingDown    = routeBinding.whenStopRequested
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
  protected val gateKeeper = GateKeeper(webServerBinding, gateKeeperConf)

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta)
  : Task[Checked[command.Response]] =
    commandExecutor.executeCommand(command, meta)

  logger.debug(s"new ControllerRoute($webServerBinding #${routeBinding.revision})")

  val webServerRoute: Route =
    (decodeRequest & encodeResponse):  // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      webLog:
        forbidCSRF:
          route

  private lazy val route =
    pathSegment("controller") {
      controllerRoute
    } ~
    serviceProviderRoute  // External service provider's routes, for example Controller's own experimental GUI

  val controllerRoute: Route =
    pathSegment("api") {
      seal:
        apiRoute
    } ~
      pathSegment("TEST"):
        passIf(config.getBoolean("js7.web.server.test")):
          testRoute


object ControllerRoute:
  private val logger = Logger[this.type]
