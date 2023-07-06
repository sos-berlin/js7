package js7.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.views.AgentOverview
import js7.base.auth.SimpleUser
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.cluster.web.ClusterNodeRouteBindings
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.SessionRegister
import js7.core.command.CommandMeta
import js7.data.event.Stamped
import js7.subagent.SubagentSession
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration.Deadline

/**
 * @author Joacim Zschimmer
 */
final class AgentRoute(
  protected val agentOverview: Task[AgentOverview],
  binding: WebServerBinding,
  protected val whenShuttingDown: Future[Deadline],
  protected val executeCommand: (AgentCommand, CommandMeta) => Task[Checked[AgentCommand.Response]],
  protected val clusterNode: ClusterNode[AgentState],
  protected val agentConfiguration: AgentConfiguration,
  gateKeeperConf: GateKeeper.Configuration[SimpleUser],
  protected val sessionRegister: SessionRegister[SubagentSession])
  (implicit
    protected val actorSystem: ActorSystem,
    protected val scheduler: Scheduler)
extends WebLogDirectives
with ApiRoute
with ClusterNodeRouteBindings[AgentState]
{
  protected val gateKeeper = GateKeeper(binding, gateKeeperConf)

  protected val agentState = clusterNode.currentState
  protected def eventWatch = clusterNode.recoveredExtract.eventWatch
  protected def akkaAskTimeout = agentConfiguration.akkaAskTimeout
  protected def config = agentConfiguration.config
  protected def actorRefFactory = actorSystem

  protected def checkedClusterState =
    agentState
      .map(_.map(s => Stamped(s.eventId, s.clusterState)))

  protected def clusterNodeIsBackup =
    clusterNode.clusterConf.isBackup

  protected def nodeId =
    clusterNode.clusterConf.ownId

  lazy val agentRoute: Route =
    pathSegments("api") {
      apiRoute
    }
}