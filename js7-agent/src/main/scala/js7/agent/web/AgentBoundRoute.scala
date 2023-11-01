package js7.agent.web

import js7.agent.DirectAgentApi
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.web.common.AgentSession
import js7.base.auth.{SimpleUser, UserId}
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.CSRF.forbidCSRF
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.SessionRegister
import js7.core.cluster.watch.ClusterWatchRegister
import js7.core.command.CommandMeta
import js7.journal.watch.EventWatch
import monix.execution.Scheduler
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.directives.CodingDirectives.{decodeRequest, encodeResponse}
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

/**
 * @author Joacim Zschimmer
 */
private final class AgentBoundRoute(
  binding: WebServerBinding,
  protected val whenShuttingDown: Future[Deadline],
  api: CommandMeta => DirectAgentApi,
  protected val agentConfiguration: AgentConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  protected val sessionRegister: SessionRegister[AgentSession],
  protected val clusterWatchRegister: ClusterWatchRegister,
  protected val eventWatch: EventWatch)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val scheduler: Scheduler)
extends PekkoWebServer.BoundRoute
with WebLogDirectives
with ApiRoute
{
  private lazy val anonymousApi = api(CommandMeta(
    user = gateKeeperConfiguration.idToUser(UserId.Anonymous)
      .getOrElse(sys.error("Anonymous user has not been defined"))))

  protected val gateKeeper = GateKeeper(binding, gateKeeperConfiguration)

  protected def agentApi(meta: CommandMeta) = api(meta)
  protected def agentOverview = anonymousApi.overview

  protected def commandExecute(meta: CommandMeta, command: AgentCommand) =
    agentApi(meta).commandExecute(command)

  protected def commandOverview = anonymousApi.commandOverview
  protected def commandDetailed = anonymousApi.commandDetailed

  protected def pekkoAskTimeout = agentConfiguration.pekkoAskTimeout
  protected def config = agentConfiguration.config
  protected def actorRefFactory = actorSystem

  override def boundMessageSuffix = gateKeeper.secureStateString

  lazy val webServerRoute: Route =
    (decodeRequest & encodeResponse) {  // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      webLog {
        seal {
          forbidCSRF {
            agentRoute
          }
        }
      }
    }

  private lazy val agentRoute: Route =
    pathSegments("agent/api") {
      apiRoute
    }
}
