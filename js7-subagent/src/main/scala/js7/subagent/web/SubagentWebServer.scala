package js7.subagent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.base.auth.{AgentDirectorPermission, SimpleUser}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.subagent.Subagent
import js7.subagent.configuration.SubagentConf
import monix.eval.Task
import monix.execution.Scheduler

object SubagentWebServer
{
  def resource(
    subagent: Task[Subagent],
    sessionRegister: SessionRegister[SimpleSession],
    convertToDirector: Task[Unit],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem, scheduler: Scheduler)
  : Resource[Task, AkkaWebServer] =
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
          for (subagent <- subagent) yield
            new SubagentRoute(
              binding,
              whenShuttingDown,
              subagent,
              sessionRegister,
              convertToDirector,
              conf.config,
              gateKeeperConf
            ).webServerRoute

        override def toString = "Subagent web services"
      })
}
