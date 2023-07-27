package js7.subagent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.base.auth.{AgentDirectorPermission, SimpleUser}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.SessionRegister
import js7.subagent.configuration.SubagentConf
import js7.subagent.{DirectorRouteVariable, Subagent, SubagentSession}
import monix.eval.Task
import monix.execution.Scheduler

object SubagentWebServer
{
  def resource(
    subagent: Task[Subagent],
    toDirectorRoute: DirectorRouteVariable.ToRoute,
    sessionRegister: SessionRegister[SubagentSession],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem, scheduler: Scheduler)
  : Resource[Task, (AkkaWebServer)] =
    AkkaWebServer.resource(
      conf.webServerBindings,
      conf.config,
      routeBinding => new AkkaWebServer.BoundRoute {
        private val gateKeeperConf =
          GateKeeper.Configuration.fromConfig(conf.config, SimpleUser.apply, Seq(
            AgentDirectorPermission))

        def serviceName = "Subagent"

        def startupSecurityHint(scheme: WebServerBinding.Scheme) =
          gateKeeperConf.secureStateString(scheme)

        lazy val webServerRoute =
          subagent
            .map(subagent =>
              new SubagentRoute(
                routeBinding, gateKeeperConf, sessionRegister,
                toDirectorRoute(routeBinding),
                subagent, conf.config
              ).webServerRoute)
            .memoize

        override def toString = "Subagent web services"
      })
}
