package js7.subagent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.server.Directives.{Segment, complete, pathPrefix}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.CodingDirectives.{decodeRequest, encodeResponse}
import com.typesafe.config.Config
import js7.base.auth.{AgentDirectorPermission, SimpleUser}
import js7.base.stream.Numbered
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.AkkaWebServer.BoundRoute
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SessionRoute, SimpleSession}
import js7.data.subagent.SubagentCommand
import js7.journal.watch.EventWatch
import js7.subagent.SubagentCommandExecutor
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

private final class SubagentBoundRoute(
  binding: WebServerBinding,
  protected val whenShuttingDown: Future[Deadline],
  protected val eventWatch: EventWatch,
  protected val commandExecutor: SubagentCommandExecutor,
  protected val sessionRegister: SessionRegister[SimpleSession],
  protected val restartAsDirector: Task[Unit],
  protected val config: Config)
  (implicit
    protected val scheduler: Scheduler,
    protected val actorSystem: ActorSystem)
extends BoundRoute
with WebLogDirectives
with CommandRoute
with SessionRoute
with EventRoute
with PseudoAgentRoute
{
  protected val actorRefFactory = actorSystem

  protected val gateKeeper = GateKeeper(
    binding,
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply, Seq(
      AgentDirectorPermission)))

  override val boundMessageSuffix = gateKeeper.secureStateString

  protected def executeCommand(command: Numbered[SubagentCommand]) =
    commandExecutor.executeCommand(command)

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
      pathPrefix(Segment) {
        case "command" => commandRoute
        case "event" => eventRoute
        case "session" => sessionRoute
        case _ => complete(NotFound)
      })
}
