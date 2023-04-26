package js7.agent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.views.AgentOverview
import js7.agent.web.common.AgentSession
import js7.base.auth.SimpleUser
import js7.base.problem.Checked
import js7.cluster.ClusterNode
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.SessionRegister
import js7.core.command.CommandMeta
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import monix.execution.Scheduler

object AgentWebServer
{
  def resource(
    agentOverview: Task[AgentOverview],
    agentConfiguration: AgentConfiguration,
    gateKeeperConf: GateKeeper.Configuration[SimpleUser],
    executeCommand: (AgentCommand, CommandMeta) => Task[Checked[AgentCommand.Response]],
    clusterNode: ClusterNode[AgentState],
    sessionRegister: SessionRegister[AgentSession],
    eventWatch: FileEventWatch)
    (implicit actorSystem: ActorSystem, scheduler: Scheduler)
  : Resource[Task, AkkaWebServer & AkkaWebServer.HasUri] =
    AkkaWebServer.resource(
      agentConfiguration.webServerBindings,
      agentConfiguration.config,
      (binding, whenShuttingDown) => new AkkaWebServer.BoundRoute {

        def startupSecurityHint(scheme: WebServerBinding.Scheme) =
          gateKeeperConf.secureStateString(scheme)

        def webServerRoute =
          Task.pure(
            new AgentRoute(
              agentOverview,
              binding,
              whenShuttingDown,
              executeCommand,
              clusterNode,
              agentConfiguration,
              gateKeeperConf,
              sessionRegister,
              eventWatch
            ).webServerRoute)
      })
}
