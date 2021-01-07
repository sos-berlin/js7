package js7.controller.web

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.config.Config
import js7.base.auth.SimpleUser
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import js7.common.akkahttp.web.auth.GateKeeper
import js7.controller.web.controller.ControllerRoute
import js7.controller.web.serviceprovider.ServiceProviderRoute

/**
  * @author Joacim Zschimmer
  */
trait CompleteRoute extends ServiceProviderRoute with ControllerRoute with WebLogDirectives {

  override protected def gateKeeper: GateKeeper[SimpleUser]
  protected implicit def actorRefFactory: ActorRefFactory
  protected def config: Config
  protected def actorSystem: ActorSystem

  final lazy val completeRoute: Route =
    (decodeRequest & encodeResponse) {  // Before handleErrorAndLog to allow simple access to HttpEntity.Strict
      webLog {
        forbidCSRF {
          route
        }
      }
    }

  private lazy val route =
    pathSegment("controller") {
      controllerRoute
    } ~
    serviceProviderRoute  // External service provider's routes, for example Controller's own experimantal GUI
}
