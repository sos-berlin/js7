package js7.subagent.web

import org.apache.pekko.actor.ActorSystem
import cats.effect.Resource
import js7.base.auth.{AgentDirectorPermission, SimpleUser}
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.SessionRegister
import js7.subagent.configuration.SubagentConf
import js7.subagent.{DirectorRouteVariable, Subagent, SubagentSession}
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.catsutils.UnsafeMemoizable.memoize

object SubagentWebServer:
  def resource(
    subagent: IO[Subagent],
    toDirectorRoute: DirectorRouteVariable.ToRoute,
    sessionRegister: SessionRegister[SubagentSession],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem, ioRuntime: IORuntime)
  : Resource[IO, (PekkoWebServer)] =
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
          memoize:
            subagent.map: subagent =>
              new SubagentRoute(
                routeBinding, gateKeeperConf, sessionRegister,
                toDirectorRoute(routeBinding),
                subagent, conf.config
              ).webServerRoute

        override def toString = "Subagent web services"
      })
