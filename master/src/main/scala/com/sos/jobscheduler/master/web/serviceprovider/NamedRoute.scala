package com.sos.jobscheduler.master.web.serviceprovider

import akka.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
final case class NamedRoute(suburi: String, route: Route)
