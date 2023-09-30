package js7.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.views.AgentOverview
import js7.base.auth.SimpleUser
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.cluster.web.ClusterNodeRouteBindings
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.AkkaWebServer.RouteBinding
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.SessionRegister
import js7.core.command.CommandMeta
import js7.data.event.Stamped
import js7.subagent.SubagentSession
import monix.eval.Task
import monix.execution.Scheduler

/**
 * @author Joacim Zschimmer
 */
final class AgentRoute(
  protected val agentOverview: Task[AgentOverview],
  routeBinding: RouteBinding,
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
with ClusterNodeRouteBindings[AgentState]:

  import routeBinding.webServerBinding

  protected def whenShuttingDown = routeBinding.whenStopRequested
  protected val gateKeeper = GateKeeper(webServerBinding, gateKeeperConf)

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

  Logger[this.type].debug(s"new AgentRoute($webServerBinding #${routeBinding.revision})")

  lazy val agentRoute: Route =
    pathSegments("api"):
      apiRoute
