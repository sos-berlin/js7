package js7.common.web.serviceprovider

import com.typesafe.config.Config

/** A Java service providing more routes for the Controller web services. */
trait RouteService:
  def newRouteMapper(config: Config): RouteMapper
