package js7.subagent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.base.auth.{AgentDirectorPermission, SimpleUser}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.subagent.SubagentCommandExecutor
import js7.subagent.configuration.SubagentConf
import monix.eval.Task
import monix.execution.Scheduler

object SubagentWebServer
{
  def resource(
    commandExecutor: Task[SubagentCommandExecutor],
    sessionRegister: SessionRegister[SimpleSession],
    convertToDirector: Task[Unit],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem, scheduler: Scheduler)
  : Resource[Task, AkkaWebServer & AkkaWebServer.HasUri] =
    AkkaWebServer.resource(
      conf.webServerBindings,
      conf.config,
      (binding, whenShuttingDown) => new AkkaWebServer.BoundRoute {
        private val gateKeeperConf =
          GateKeeper.Configuration.fromConfig(conf.config, SimpleUser.apply, Seq(
            AgentDirectorPermission))

        def startupSecurityHint(scheme: WebServerBinding.Scheme) =
          gateKeeperConf.secureStateString(scheme)

        val webServerRoute =
          for (commandExecutor <- commandExecutor) yield
            new SubagentRoute(
              binding,
              whenShuttingDown,
              commandExecutor,
              sessionRegister,
              convertToDirector,
              conf.config,
              gateKeeperConf
            ).webServerRoute
      })
}
