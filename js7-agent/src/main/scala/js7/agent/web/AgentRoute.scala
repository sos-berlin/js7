package js7.agent.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.base.auth.SimpleUser
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.cluster.web.ClusterNodeRouteBindings
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.PekkoWebServer.RouteBinding
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.SessionRegister
import js7.core.command.CommandMeta
import js7.data.event.Stamped
import js7.subagent.SubagentSession
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Route

/**
 * @author Joacim Zschimmer
 */
final class AgentRoute(
  routeBinding: RouteBinding,
  protected val executeCommand: (AgentCommand, CommandMeta) => IO[Checked[AgentCommand.Response]],
  protected val clusterNode: ClusterNode[AgentState],
  protected val agentConfiguration: AgentConfiguration,
  gateKeeperConf: GateKeeper.Configuration[SimpleUser],
  protected val sessionRegister: SessionRegister[SubagentSession])
  (implicit
    protected val actorSystem: ActorSystem,
    protected val ioRuntime: IORuntime)
extends WebLogDirectives, ApiRoute, ClusterNodeRouteBindings[AgentState]:

  import routeBinding.webServerBinding

  protected def whenShuttingDown = routeBinding.whenStopRequested
  protected val gateKeeper = GateKeeper(webServerBinding, gateKeeperConf)

  protected val agentState = clusterNode.currentState
  protected def eventWatch = clusterNode.recoveredExtract.eventWatch
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
    pathSegments("api"):
      apiRoute
