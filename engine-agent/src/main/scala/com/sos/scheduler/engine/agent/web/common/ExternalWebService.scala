package com.sos.scheduler.engine.agent.web.common

import spray.routing.Route

/**
 *
 * A web service defined outside the Agent.
 * @author Joacim Zschimmer
 */
trait ExternalWebService {
  def route: Route
}
