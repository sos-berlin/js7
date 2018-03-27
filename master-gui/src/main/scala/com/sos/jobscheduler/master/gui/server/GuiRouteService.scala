package com.sos.jobscheduler.master.gui.server

import com.sos.jobscheduler.master.web.master.RouteService
import com.sos.jobscheduler.master.web.master.RouteService.NamedRoute

/**
  * @author Joacim Zschimmer
  */
final class GuiRouteService extends RouteService with GuiRoute
{
  def namedRoutes = Vector(
    NamedRoute("", indexHtmlRoute),  // /master
    NamedRoute("gui", guiRoute))     // /master/gui
}

