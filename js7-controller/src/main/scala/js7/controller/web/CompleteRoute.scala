package js7.controller.web

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import js7.controller.web.controller.ControllerRoute
import js7.controller.web.serviceprovider.ServiceProviderRoute

/**
  * @author Joacim Zschimmer
  */
trait CompleteRoute extends ServiceProviderRoute with ControllerRoute with WebLogDirectives {

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
    serviceProviderRoute  // External service provider's routes, for example Controller's own experimental GUI
}
