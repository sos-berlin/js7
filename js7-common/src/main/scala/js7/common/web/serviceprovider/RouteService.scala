package js7.common.web.serviceprovider

import js7.common.configuration.CommonConfiguration

/** A Java service providing more routes for the Controller web services. */
trait RouteService:
  def newRouteMapper(conf: CommonConfiguration): RouteMapper
