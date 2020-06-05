package js7.master.web

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.config.Config
import js7.base.auth.SimpleUser
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import js7.common.akkahttp.web.auth.GateKeeper
import js7.master.web.master.MasterRoute
import js7.master.web.serviceprovider.ServiceProviderRoute

/**
  * @author Joacim Zschimmer
  */
trait CompleteRoute extends ServiceProviderRoute with MasterRoute with WebLogDirectives {

  override protected def gateKeeper: GateKeeper[SimpleUser]
  protected implicit def actorRefFactory: ActorRefFactory
  protected def config: Config
  protected def actorSystem: ActorSystem

  final lazy val completeRoute: Route =
    (decodeRequest & encodeResponse) {  // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      webLog(userId = None) {
        forbidCSRF {
          route
        }
      }
    }

  private lazy val route =
    pathSegment("master") {
      masterRoute
    } ~
    serviceProviderRoute  // External service provider's routes, for example Master's own experimantal GUI
}
