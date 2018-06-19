package com.sos.jobscheduler.master.web.common

import com.sos.jobscheduler.common.akkahttp.web.session.{RouteProvider, SimpleSession}

/**
  * @author Joacim Zschimmer
  */
trait MasterRouteProvider extends RouteProvider {
  protected type Session = SimpleSession
}
