package js7.service.prometheus

import js7.base.utils.ScalaUtils.syntax.*
import js7.common.configuration.CommonConfiguration
import js7.common.web.serviceprovider.RouteService

final class PrometheusRouteService extends RouteService:

  def newRouteMapper(conf: CommonConfiguration) =
    new PrometheusRouteMapper(conf)

  override def toString = getClass.simpleScalaName
