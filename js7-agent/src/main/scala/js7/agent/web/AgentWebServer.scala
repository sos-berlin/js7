package js7.agent.web

import akka.actor.ActorSystem
import js7.agent.DirectAgentApi
import js7.agent.configuration.AgentConfiguration
import js7.base.auth.SimpleUser
import js7.base.generic.Completed
import js7.base.utils.SetOnce
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.core.cluster.ClusterWatchRegister
import js7.core.command.CommandMeta
import js7.journal.watch.EventWatch
import monix.eval.Task
import monix.execution.Scheduler

object AgentWebServer
{
  def apply(
    agentConfiguration: AgentConfiguration,
    gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    sessionRegister: SessionRegister[SimpleSession],
    clusterWatchRegister: ClusterWatchRegister,
    eventWatch: EventWatch)
    (implicit
      actorSystem: ActorSystem,
      scheduler: Scheduler)
  : AkkaWebServer with AkkaWebServer.HasUri with StartableWithApi = {
    val apiOnce = SetOnce[CommandMeta => DirectAgentApi]

    new AkkaWebServer.Standard(
      agentConfiguration.webServerBindings,
      agentConfiguration.config,
      (binding, whenShuttingDown) =>
        Task.deferAction(implicit scheduler => Task.pure(
          new AgentBoundRoute(
            binding,
            whenShuttingDown,
            apiOnce.orThrow,
            agentConfiguration,
            gateKeeperConfiguration,
            sessionRegister,
            clusterWatchRegister,
            eventWatch))))
      with StartableWithApi
      {
        def start(api: CommandMeta => DirectAgentApi) =
          Task.defer {
            apiOnce := api
            super.start
          }
      }
  }

  sealed trait StartableWithApi {
    def start(api: CommandMeta => DirectAgentApi): Task[Completed]
  }
}
