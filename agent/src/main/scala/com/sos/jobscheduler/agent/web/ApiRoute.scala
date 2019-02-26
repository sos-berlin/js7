package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.web.views.RootWebService
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRoute

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends RootWebService
with CommandWebService
with MastersEventWebService
with OrderWebService
//with TaskWebService
with SessionRoute
{
  private lazy val masterRoute =
    pathSegment("event") {
      masterEventRoute
    }

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
