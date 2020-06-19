package js7.agent.web.controller

import akka.http.scaladsl.server.RouteConcatenation._
import js7.base.auth.ValidUserPermission
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.core.cluster.ControllersClusterRoute
import js7.data.controller.ControllerId

trait ControllerRoute extends ControllersEventRoute with ControllersClusterRoute
{
  protected final lazy val controllerRoute =
    pathSegment("event") {
      controllerEventRoute
    } ~
    pathSegment("cluster") {
      authorizedUser(ValidUserPermission) { user =>
        controllerClusterRoute(ControllerId.fromUserId(user.id))
      }
    }
}
