package js7.subagent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.server.Directives.{Segment, complete, pathEndOrSingleSlash, pathPrefix}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.RouteConcatenation.*
import akka.http.scaladsl.server.directives.CodingDirectives.{decodeRequest, encodeResponse}
import com.typesafe.config.Config
import js7.base.BuildInfo
import js7.base.auth.SimpleUser
import js7.base.stream.Numbered
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.CirceJsonSupport.jsonMarshaller
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SessionRoute, SimpleSession}
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.common.system.startup.StartUp
import js7.data.subagent.{SubagentCommand, SubagentOverview}
import js7.subagent.SubagentCommandExecutor
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

private final class SubagentRoute(
  binding: WebServerBinding,
  protected val whenShuttingDown: Future[Deadline],
  subagentCommandExecuter: SubagentCommandExecutor,
  protected val sessionRegister: SessionRegister[SimpleSession],
  protected val convertToDirector: Task[Unit],
  protected val config: Config,
  gateKeeperConf: GateKeeper.Configuration[SimpleUser])
  (implicit
    protected val scheduler: Scheduler,
    protected val actorSystem: ActorSystem)
extends WebLogDirectives
with CommandRoute
with SessionRoute
with EventRoute
with PseudoDirectorRoute
{
  protected val subagent = subagentCommandExecuter.subagent
  protected val eventWatch = subagent.journal.eventWatch
  protected val actorRefFactory = actorSystem

  protected val gateKeeper = GateKeeper(binding, gateKeeperConf)

  protected def executeCommand(command: Numbered[SubagentCommand]) =
    subagentCommandExecuter.executeCommand(command)

  protected def overviewRoute: Route =
    complete(SubagentOverview(
      version = BuildInfo.prettyVersion,
      buildId = BuildInfo.buildId,
      startedAt = StartUp.startedAt,
      isTerminating = subagent.isShuttingDown,
      system = systemInformation(),
      java = javaInformation()))

  val webServerRoute: Route =
    (decodeRequest & encodeResponse)( // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      webLog(seal(forbidCSRF(route))))

  private lazy val route =
    pathPrefix(Segment) {
      case "subagent" => subagentRoute
      case "agent" => pseudoAgentRoute
      case _ => complete(NotFound)
    }

  private lazy val subagentRoute: Route =
    pathSegment("api")(
      pathEndOrSingleSlash(overviewRoute) ~
        pathPrefix(Segment) {
          case "command" => commandRoute
          case "event" => eventRoute
          case "session" => sessionRoute
          case _ => complete(NotFound)
        })
}
