package com.sos.jobscheduler.master.web.master

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.html.{HtmlDirectives, WebServiceContext}
import com.sos.jobscheduler.master.web.master.api.ApiRoute
import com.sos.jobscheduler.master.web.master.gui.GuiRoute

/**
  * @author Joacim Zschimmer
  */
trait MasterRoute extends HtmlDirectives[WebServiceContext] with ApiRoute with GuiRoute {

  final val masterRoute: Route =
    pathEndElseRedirect {
      indexHtmlRoute
    } ~
    pathSegments("api") {
      apiRoute
    } ~
    pathSegments("gui") {
      guiRoute
    }
}
