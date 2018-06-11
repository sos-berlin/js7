package com.sos.jobscheduler.master.web.serviceprovider

import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait RouteService {
  def namedRoutes: Seq[NamedRoute]
}
