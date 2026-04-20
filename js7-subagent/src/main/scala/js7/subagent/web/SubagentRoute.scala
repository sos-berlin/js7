package js7.subagent.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.nio.file.Path
import js7.base.BuildInfo
import js7.base.auth.SimpleUser
import js7.base.log.Logger
import js7.base.stream.Numbered
import js7.base.system.SystemInformations.systemInformation
import js7.base.system.startup.StartUp
import js7.common.metrics.MetricsRoute
import js7.common.pekkohttp.CirceJsonSupport.jsonMarshaller
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.common.pekkohttp.StandardDirectives.ioRoute
import js7.common.pekkohttp.StandardMarshallers.StatusCodeMarshaller
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer.RouteBinding
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.{SessionRegister, SessionRoute}
import js7.common.system.JavaInformations.javaInformation
import js7.common.web.serviceprovider.ServiceProviderRoute
import js7.core.command.CommandMeta
import js7.core.web.log.LogRoute
import js7.data.subagent.{SubagentCommand, SubagentOverview}
import js7.subagent.DirectorRouteVariable.DirectorRoutes
import js7.subagent.web.SubagentRoute.*
import js7.subagent.{Subagent, SubagentSession}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.server.Directives.{Segment, complete, pathEndOrSingleSlash, pathPrefix, reject}
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.RouteConcatenation.*

private final class SubagentRoute(
  routeBinding: RouteBinding,
  gateKeeperConf: GateKeeper.Configuration[SimpleUser],
  protected val sessionRegister: SessionRegister[SubagentSession],
  directorRoute: IO[DirectorRoutes],
  protected val subagent: Subagent)
  (using
    protected val ioRuntime: IORuntime,
    protected val actorSystem: ActorSystem)
extends
  ServiceProviderRoute,
  WebLogDirectives,
  CommandRoute,
  SessionRoute,
  EventRoute,
  LogRoute,
  MetricsRoute:

  import routeBinding.webServerBinding

  protected def commonConf = subagent.conf

  protected def whenShuttingDown = routeBinding.whenStopRequested
  protected def js7ServerId = subagent.subagentId.map(_.toJs7ServerId)
  protected val logDirectory: Path = subagent.conf.logDirectory
  protected val logDirectoryIndexRegister = subagent.logDirectoryIndexRegister
  protected val eventWatch = subagent.journal.eventWatch
  protected val actorRefFactory = actorSystem
  protected val gateKeeper = GateKeeper(webServerBinding, gateKeeperConf)

  protected def executeCommand(command: Numbered[SubagentCommand], meta: CommandMeta) =
    subagent.commandExecutor.executeCommand(command, meta)

  logger.debug(s"new SubagentRoute($webServerBinding #${routeBinding.revision})")

  def webServerRoute: Route =
    mainRoute:
      pathPrefix(Segment):
        case "subagent" => subagentRoute
        case "agent" => ioRoute(directorRoute.map(_.route))
        case "metrics" => ioRoute:
          directorRoute.map(_.directorMetricsRoute getOrElse metricsRoute)
        case _ => reject
      ~ serviceProviderRoute

  private lazy val subagentRoute: Route =
    pathSegment("api"):
      pathEndOrSingleSlash(overviewRoute) ~
        pathPrefix(Segment):
          case "command" => commandRoute
          case "event" => eventRoute
          case "session" => sessionRoute
          case "log" => logRoute
          case _ => complete(NotFound)

  private def overviewRoute: Route =
    complete(SubagentOverview(
      version = BuildInfo.prettyVersion,
      buildId = BuildInfo.buildId,
      startedAt = StartUp.startedAt,
      isTerminating = subagent.isShuttingDown,
      system = systemInformation(),
      java = javaInformation()))


object SubagentRoute:
  private val logger = Logger[this.type]
