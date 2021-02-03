package js7.agent.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.agent.web.controller.ControllerRoute
import js7.agent.web.views.RootWebService
import js7.base.auth.ValidUserPermission
import js7.common.akkahttp.web.session.SessionRoute
import js7.data.controller.ControllerId

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends RootWebService
with CommandWebService
with ControllerRoute
with OrderWebService
with SessionRoute
{
  protected final val apiRoute: Route =
    pathPrefix(Segment) {
      case "controller" => controllerRoute
      case "command"    => commandRoute
      case "order"      => orderRoute
      case "clusterWatch" =>
        authorizedUser(ValidUserPermission) { user =>
          clusterWatchRoute(ControllerId.fromUserId(user.id))
        }
      case "session"    => sessionRoute
      case _ => complete(NotFound)
    } ~
    pathEnd {
      apiRootRoute
    }
}
