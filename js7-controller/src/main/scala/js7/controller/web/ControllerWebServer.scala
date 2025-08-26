package js7.controller.web

import cats.effect.ResourceIO
import js7.base.auth.{AgentDirectorForwardPermission, SimpleUser, UpdateItemPermission}
import js7.cluster.ClusterNode
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.controller.OrderApi
import js7.controller.command.ControllerCommandExecutor
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.ItemUpdater
import js7.data.controller.ControllerState
import js7.journal.watch.FileEventWatch
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.Deadline

object ControllerWebServer:

  def resource(
    orderApi: OrderApi,
    commandExecutor: ControllerCommandExecutor,
    itemUpdater: ItemUpdater,
    clusterNode: ClusterNode[ControllerState],
    totalRunningSince: Deadline,
    eventWatch: FileEventWatch,
    conf: ControllerConfiguration,
    sessionRegister: SessionRegister[SimpleSession])(
    implicit actorSystem_ : ActorSystem, ioRuntime: IORuntime)
  : ResourceIO[ControllerWebServer] =
    PekkoWebServer.service(conf.webServerBindings, conf.config): routeBinding =>
      new PekkoWebServer.BoundRoute:
        import conf.config

        private val gateKeeperConf =
          GateKeeper.Configuration.fromConfig(config, SimpleUser.apply,
            Set(UpdateItemPermission, AgentDirectorForwardPermission))

        def serviceName = "Controller"

        def startupSecurityHint(scheme: WebServerBinding.Scheme) =
          gateKeeperConf.secureStateString(scheme)

        def webServerRoute =
          IO.pure(
            new ControllerRoute(
              routeBinding,
              conf,
              orderApi,
              commandExecutor,
              itemUpdater,
              clusterNode,
              totalRunningSince,
              sessionRegister,
              eventWatch,
              gateKeeperConf
            ).webServerRoute)

        override def toString = "Controller web services"
