package js7.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.CodingDirectives.{decodeRequest, encodeResponse}
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.views.AgentOverview
import js7.agent.web.common.AgentSession
import js7.base.auth.SimpleUser
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.cluster.web.ClusterNodeRouteBindings
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.SessionRegister
import js7.core.command.CommandMeta
import js7.data.event.Stamped
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

/**
 * @author Joacim Zschimmer
 */
private final class AgentBoundRoute(
  protected val agentOverview: Task[AgentOverview],
  binding: WebServerBinding,
  protected val whenShuttingDown: Future[Deadline],
  protected val executeCommand: (AgentCommand, CommandMeta) => Task[Checked[AgentCommand.Response]],
  protected val clusterNode: ClusterNode[AgentState],
  protected val agentConfiguration: AgentConfiguration,
  gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
  protected val sessionRegister: SessionRegister[AgentSession],
  protected val eventWatch: FileEventWatch)
  (implicit
    protected val actorSystem: ActorSystem,
    protected val scheduler: Scheduler)
extends AkkaWebServer.BoundRoute
with WebLogDirectives
with ApiRoute
with ClusterNodeRouteBindings[AgentState]
{
  protected val gateKeeper = GateKeeper(binding, gateKeeperConfiguration)

  protected val agentState = clusterNode.currentState
  protected def akkaAskTimeout = agentConfiguration.akkaAskTimeout
  protected def config = agentConfiguration.config
  protected def actorRefFactory = actorSystem

  def boundMessageSuffix = gateKeeper.secureStateString

  protected def checkedClusterState =
    agentState
      .map(_.map(s => Stamped(s.eventId, s.clusterState)))

  protected def clusterNodeIsBackup =
    clusterNode.clusterConf.isBackup

  protected def nodeId =
    clusterNode.clusterConf.ownId

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
