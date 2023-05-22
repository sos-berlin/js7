package js7.subagent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.base.auth.{AgentDirectorPermission, SimpleUser}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.subagent.configuration.SubagentConf
import js7.subagent.{DirectorRouteVariable, Subagent}
import monix.eval.Task
import monix.execution.Scheduler

object SubagentWebServer
{
  def resource(
    subagent: Task[Subagent],
    toDirectorRoute: DirectorRouteVariable.ToRoute,
    sessionRegister: SessionRegister[SimpleSession],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem, scheduler: Scheduler)
  : Resource[Task, (AkkaWebServer)] =
    AkkaWebServer.resource(
      conf.webServerBindings,
      conf.config,
      (binding, whenShuttingDown) => new AkkaWebServer.BoundRoute {
        private val gateKeeperConf =
          GateKeeper.Configuration.fromConfig(conf.config, SimpleUser.apply, Seq(
            AgentDirectorPermission))

        def startupSecurityHint(scheme: WebServerBinding.Scheme) =
          gateKeeperConf.secureStateString(scheme)

        def webServerRoute =
          for (subagent <- subagent) yield
            new SubagentRoute(binding, whenShuttingDown, gateKeeperConf, sessionRegister,
              toDirectorRoute(binding, whenShuttingDown),
              subagent, conf.config
            ).webServerRoute

        override def toString = "Subagent web services"
      })
}
