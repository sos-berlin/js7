package js7.subagent.web

import com.typesafe.config.Config
import js7.base.BuildInfo
import js7.base.auth.SimpleUser
import js7.base.log.Logger
import js7.base.stream.Numbered
import js7.common.pekkohttp.CirceJsonSupport.jsonMarshaller
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.common.pekkohttp.StandardDirectives.taskRoute
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer.RouteBinding
import js7.common.pekkohttp.web.auth.CSRF.forbidCSRF
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.{SessionRegister, SessionRoute}
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.common.system.startup.StartUp
import js7.core.command.CommandMeta
import js7.data.subagent.{SubagentCommand, SubagentOverview}
import js7.subagent.web.SubagentRoute.*
import js7.subagent.{Subagent, SubagentSession}
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.server.Directives.{Segment, complete, pathEndOrSingleSlash, pathPrefix}
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.RouteConcatenation.*
import org.apache.pekko.http.scaladsl.server.directives.CodingDirectives.{decodeRequest, encodeResponse}

private final class SubagentRoute(
  routeBinding: RouteBinding,
  gateKeeperConf: GateKeeper.Configuration[SimpleUser],
  protected val sessionRegister: SessionRegister[SubagentSession],
  directorRoute: Task[Route],
  protected val subagent: Subagent,
  protected val config: Config)
  (implicit
    protected val scheduler: Scheduler,
    protected val actorSystem: ActorSystem)
extends WebLogDirectives
with CommandRoute
with SessionRoute
with EventRoute
{
  import routeBinding.webServerBinding

  protected def whenShuttingDown = routeBinding.whenStopRequested
  protected val eventWatch = subagent.journal.eventWatch
  protected val actorRefFactory = actorSystem
  protected val gateKeeper = GateKeeper(webServerBinding, gateKeeperConf)

  protected def executeCommand(command: Numbered[SubagentCommand], meta: CommandMeta) =
    subagent.commandExecutor.executeCommand(command, meta)

  logger.debug(s"new SubagentRoute($webServerBinding #${routeBinding.revision})")

  def webServerRoute: Route =
    (decodeRequest & encodeResponse)( // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      webLog(seal(forbidCSRF(route))))

  private lazy val route =
    pathPrefix(Segment) {
      case "subagent" => subagentRoute
      case "agent" => taskRoute(directorRoute)
      case _ => complete(NotFound)
    }

  lazy val subagentRoute: Route =
    pathSegment("api")(
      pathEndOrSingleSlash(overviewRoute) ~
        pathPrefix(Segment) {
          case "command" => commandRoute
          case "event" => eventRoute
          case "session" => sessionRoute
          case _ => complete(NotFound)
        })

  private def overviewRoute: Route =
    complete(SubagentOverview(
      version = BuildInfo.prettyVersion,
      buildId = BuildInfo.buildId,
      startedAt = StartUp.startedAt,
      isTerminating = subagent.isShuttingDown,
      system = systemInformation(),
      java = javaInformation()))
}

object SubagentRoute {
  private val logger = Logger[this.type]
}
