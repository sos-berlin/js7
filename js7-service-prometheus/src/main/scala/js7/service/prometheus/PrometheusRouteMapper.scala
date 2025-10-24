package js7.service.prometheus

import js7.base.auth.ReadMetricsPermission
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.configuration.CommonConfiguration
import js7.common.web.serviceprovider.{RouteMapper, RouteServiceContext}

private[prometheus] final class PrometheusRouteMapper(protected val conf: CommonConfiguration)
extends RouteMapper, PrometheusMetricsRoute:

  PrometheusRouteMapper  // Force logging

  def pathToRoute(context: RouteServiceContext) = Map(
    "metrics" -> (metricsRoute, Set(ReadMetricsPermission)))

  override def toString = getClass.simpleScalaName


private[prometheus] object PrometheusRouteMapper:
  private val logger = Logger[this.type]

  logger.info("Providing /metrics web service for Prometheus")
