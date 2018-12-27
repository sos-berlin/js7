package com.sos.jobscheduler.master.web.serviceprovider

import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.StandardDirectives.combineRoutes
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.master.web.serviceprovider.ServiceProviderRoute._
import java.util.ServiceLoader
import monix.execution.Scheduler
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
private[web] trait ServiceProviderRoute
{
  protected def scheduler: Scheduler
  protected def injector: Injector

  private lazy val services: Seq[RouteService] = {
    val services = ServiceLoader.load(classOf[RouteService]).iterator.asScala.toVector
    if (services.isEmpty) logger.debug("No services providers")
    else for (s ← services) {
      logger.debug(s"Found service provider ${s.getClass.scalaName}")
      injector.injectMembers(s)
    }
    services
  }

  final lazy val serviceProviderRoute: Route = {
    val namedRouteToService = for (s ← services; r ← s.namedRoutes) yield r → s
    logAndCheck(namedRouteToService)
    combineRoutes(
      namedRouteToService map (_._1) map (r ⇒ pathSegments(r.suburi)(r.route)))
  }
}

private[web] object ServiceProviderRoute
{
  private val logger = Logger(getClass)

  private def logAndCheck(namedRouteToService: Seq[(NamedRoute, RouteService)]): Unit = {
    for ((r, s) ← namedRouteToService) logger.debug(s"${s.getClass.scalaName} provides route '${r.suburi}'")
    if (namedRouteToService.isEmpty) logger.trace(s"No routes")
    for (duplicates ← namedRouteToService.duplicateKeys(_._1.suburi)) {
      sys.error("Duplicate RouteService: " + duplicates.values.flatten.map(o ⇒ s"'${o._1.suburi}'->${o._2.getClass.scalaName}" ).mkString(", "))
    }
  }
}
