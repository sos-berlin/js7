package js7.controller.web.common

import js7.common.akkahttp.web.session.{RouteProvider, SimpleSession}

/**
  * @author Joacim Zschimmer
  */
trait ControllerRouteProvider extends RouteProvider {
  protected type OurSession = SimpleSession
}
