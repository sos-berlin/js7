package com.sos.jobscheduler.master.gui.server

import akka.http.scaladsl.model.StatusCodes.TemporaryRedirect
import akka.http.scaladsl.server.Directives.{pathEndOrSingleSlash, redirect}
import com.sos.jobscheduler.common.akkahttp.html.HtmlDirectives.htmlPreferred
import com.sos.jobscheduler.master.web.serviceprovider.{NamedRoute, RouteService}
import com.typesafe.config.Config
import javax.inject.Inject

/**
  * @author Joacim Zschimmer
  */
final class GuiRouteService extends RouteService with GuiRoute
{
  @Inject
  protected var config: Config = null

  def namedRoutes = Vector(
    NamedRoute("",
      pathEndOrSingleSlash {
        htmlPreferred {
          redirect("/master", TemporaryRedirect)
        }
      }
    ),
    NamedRoute("master", indexHtmlRoute),
    NamedRoute("master/gui", guiRoute))
}

