package js7.agent.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.agent.web.master.MasterRoute
import js7.agent.web.views.RootWebService
import js7.common.akkahttp.web.session.SessionRoute

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends RootWebService
with CommandWebService
with MasterRoute
with OrderWebService
//with TaskWebService
with SessionRoute
{
  protected final val apiRoute: Route =
    pathPrefix(Segment) {
      case "master"  => masterRoute
      case "command" => commandRoute
      case "order"   => orderRoute
      //case "task"  => taskRoute
      case "session" => sessionRoute
      case _ => complete(NotFound)
    } ~
    pathEnd {
      apiRootRoute
    }
}
