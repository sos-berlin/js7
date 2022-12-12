package js7.agent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.agent.DirectAgentApi
import js7.agent.configuration.AgentConfiguration
import js7.agent.web.common.AgentSession
import js7.base.auth.SimpleUser
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.SessionRegister
import js7.core.command.CommandMeta
import js7.journal.watch.EventWatch
import monix.eval.Task

object AgentWebServer
{
  def resource(
    agentConfiguration: AgentConfiguration,
    gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    api: CommandMeta => DirectAgentApi,
    sessionRegister: SessionRegister[AgentSession],
    eventWatch: EventWatch)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer & AkkaWebServer.HasUri] =
    AkkaWebServer.resource(
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
            eventWatch))))
}
