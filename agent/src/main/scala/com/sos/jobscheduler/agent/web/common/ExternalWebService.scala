package com.sos.jobscheduler.agent.web.common

/**
 *
 * A web service defined outside the Agent.
  *
  * @author Joacim Zschimmer
 */
trait ExternalWebService {
  def routeBuilder: RouteBuilder
}
