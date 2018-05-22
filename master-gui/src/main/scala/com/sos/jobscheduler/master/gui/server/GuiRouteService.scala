package com.sos.jobscheduler.master.gui.server

import com.sos.jobscheduler.master.web.master.RouteService
import com.sos.jobscheduler.master.web.master.RouteService.NamedRoute
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
    NamedRoute("", indexHtmlRoute),  // /master
    NamedRoute("gui", guiRoute))     // /master/gui
}

