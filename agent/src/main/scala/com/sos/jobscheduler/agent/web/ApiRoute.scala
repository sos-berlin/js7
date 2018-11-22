package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.web.views.RootWebService
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
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
  protected final val apiRoute: Route =
    pathSegments("master/event") {
      masterEventRoute
    } ~
    pathSegments("command") {
      commandRoute
    } ~
    pathSegments("order") {
      orderRoute
    } ~
    //pathSegments("task") {
    //  taskRoute
    //} ~
    pathSegments("session") {
      sessionRoute
    } ~
    pathEnd {
      apiRootRoute
    }
}
