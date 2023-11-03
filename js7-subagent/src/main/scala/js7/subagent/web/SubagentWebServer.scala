package js7.subagent.web

import cats.effect.Resource
import js7.base.auth.{AgentDirectorPermission, SimpleUser}
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.SessionRegister
import js7.subagent.configuration.SubagentConf
import js7.subagent.{DirectorRouteVariable, Subagent, SubagentSession}
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.actor.ActorSystem

object SubagentWebServer
{
  def resource(
    subagent: Task[Subagent],
    toDirectorRoute: DirectorRouteVariable.ToRoute,
    sessionRegister: SessionRegister[SubagentSession],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem, scheduler: Scheduler)
  : Resource[Task, (PekkoWebServer)] =
    PekkoWebServer.resource(
      conf.webServerBindings,
      conf.config,
      routeBinding => new PekkoWebServer.BoundRoute {
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
