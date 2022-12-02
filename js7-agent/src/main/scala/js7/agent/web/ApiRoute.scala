package js7.agent.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import js7.agent.web.views.RootWebService
import js7.common.akkahttp.web.session.SessionRoute

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends RootWebService
with CommandWebService
with EventRoute
with SessionRoute
{
  protected final val apiRoute: Route =
    pathPrefix(Segment) {
      case "event" => eventRoute
      case "command" => commandRoute
      case "session" => sessionRoute
      case _ => complete(NotFound)
    } ~
    pathEndOrSingleSlash {
      apiRootRoute
    }
}
