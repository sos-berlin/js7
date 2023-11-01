package js7.controller.web.controller

import js7.common.pekkohttp.PekkoHttpServerUtils.{passIf, pathSegment}
import js7.common.pekkohttp.WebLogDirectives
import js7.controller.web.controller.api.ApiRoute
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

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
