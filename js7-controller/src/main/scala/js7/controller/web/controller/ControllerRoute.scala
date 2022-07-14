package js7.controller.web.controller

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import js7.common.akkahttp.AkkaHttpServerUtils.{passIf, pathSegment}
import js7.common.akkahttp.WebLogDirectives
import js7.controller.web.controller.api.ApiRoute

/**
  * @author Joacim Zschimmer
  */
trait ControllerRoute extends ApiRoute with WebLogDirectives with TestRoute {

  final val controllerRoute: Route =
    pathSegment("api") {
      seal {
        apiRoute
      }
    } ~
    pathSegment("TEST") {
      passIf(config.getBoolean("js7.web.server.test")) {
        testRoute
      }
    }
}
