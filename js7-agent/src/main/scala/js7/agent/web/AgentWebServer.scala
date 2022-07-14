package js7.agent.web

import akka.actor.ActorSystem
import js7.agent.DirectAgentApi
import js7.agent.configuration.AgentConfiguration
import js7.base.auth.SimpleUser
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.core.cluster.ClusterWatchRegister
import js7.core.command.CommandMeta
import js7.journal.watch.EventWatch
import monix.eval.Task

object AgentWebServer
{
  def apply(
    agentConfiguration: AgentConfiguration,
    gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    api: CommandMeta => DirectAgentApi,
    sessionRegister: SessionRegister[SimpleSession],
    clusterWatchRegister: ClusterWatchRegister,
    eventWatch: EventWatch)
    (implicit actorSystem: ActorSystem)
  : AkkaWebServer & AkkaWebServer.HasUri =
    new AkkaWebServer.Standard(
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
