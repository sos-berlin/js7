package js7.agent.web

import js7.agent.DirectAgentApi
import js7.agent.configuration.AgentConfiguration
import js7.agent.web.common.AgentSession
import js7.base.auth.SimpleUser
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.session.SessionRegister
import js7.core.cluster.watch.ClusterWatchRegister
import js7.core.command.CommandMeta
import js7.journal.watch.EventWatch
import monix.eval.Task
import org.apache.pekko.actor.ActorSystem

object AgentWebServer
{
  def apply(
    agentConfiguration: AgentConfiguration,
    gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    api: CommandMeta => DirectAgentApi,
    sessionRegister: SessionRegister[AgentSession],
    clusterWatchRegister: ClusterWatchRegister,
    eventWatch: EventWatch)
    (implicit actorSystem: ActorSystem)
  : PekkoWebServer & PekkoWebServer.HasUri =
    new PekkoWebServer.Standard(
      agentConfiguration.webServerBindings,
      agentConfiguration.config,
      (binding, whenShuttingDown) =>
        Task.deferAction(implicit scheduler => Task.pure(
          new AgentBoundRoute(
            binding,
            whenShuttingDown,
            api,
            agentConfiguration,
            gateKeeperConfiguration,
            sessionRegister,
            clusterWatchRegister,
            eventWatch))))
}
