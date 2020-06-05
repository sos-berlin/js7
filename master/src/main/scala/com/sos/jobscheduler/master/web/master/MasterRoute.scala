package js7.master.web.master

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.common.akkahttp.AkkaHttpServerUtils.{passIf, pathSegment}
import js7.common.akkahttp.WebLogDirectives
import js7.master.web.master.api.ApiRoute

/**
  * @author Joacim Zschimmer
  */
trait MasterRoute extends ApiRoute with WebLogDirectives with TestRoute {

  final val masterRoute: Route =
    pathSegment("api") {
      seal {
        apiRoute
      }
    } ~
    pathSegment("TEST") {
      passIf(config.getBoolean("jobscheduler.webserver.test")) {
        testRoute
      }
    }
}
