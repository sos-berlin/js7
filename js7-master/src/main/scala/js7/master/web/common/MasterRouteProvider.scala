package js7.master.web.common

import js7.common.akkahttp.web.session.{RouteProvider, SimpleSession}

/**
  * @author Joacim Zschimmer
  */
trait MasterRouteProvider extends RouteProvider {
  protected type Session = SimpleSession
}
