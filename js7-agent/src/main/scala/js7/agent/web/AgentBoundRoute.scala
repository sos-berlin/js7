package js7.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.CodingDirectives.{decodeRequest, encodeResponse}
import js7.agent.DirectAgentApi
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.data.views.AgentOverview
import js7.agent.web.common.AgentSession
import js7.base.auth.SimpleUser
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.SessionRegister
import js7.core.command.CommandMeta
import js7.data.cluster.{ClusterCommand, ClusterState, ClusterWatchingCommand}
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
  api: CommandMeta => DirectAgentApi,
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
{
  protected val gateKeeper = GateKeeper(binding, gateKeeperConfiguration)

  protected def commandExecute(meta: CommandMeta, command: AgentCommand) =
    api(meta).commandExecute(command)

  protected def akkaAskTimeout = agentConfiguration.akkaAskTimeout
  protected def config = agentConfiguration.config
  protected def actorRefFactory = actorSystem

  override def boundMessageSuffix = gateKeeper.secureStateString

  protected def checkedClusterState: Task[Checked[Stamped[ClusterState]]] = ???

  protected def clusterNodeIsBackup: Boolean = ???

  protected def nodeId = ???

  protected def executeClusterCommand(cmd: ClusterCommand): Task[Checked[ClusterCommand.Response]] = ???

  protected def executeClusterWatchingCommand(cmd: ClusterWatchingCommand): Task[Checked[Unit]] = ???

  protected def clusterWatchRequestStream = Task.pure(fs2.Stream.never[Task])

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
