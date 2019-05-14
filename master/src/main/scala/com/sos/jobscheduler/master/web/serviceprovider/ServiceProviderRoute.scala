package com.sos.jobscheduler.master.web.serviceprovider

import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.Lazy
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.StandardDirectives.combineRoutes
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.master.web.serviceprovider.ServiceProviderRoute._
import java.util.ServiceLoader
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
private[web] trait ServiceProviderRoute
{
  protected def injector: Injector

  private lazy val services: Seq[RouteService] = {
    val services = ServiceLoader.load(classOf[RouteService]).iterator.asScala.toVector
    if (services.isEmpty) logger.debug("No service providers")
    else for (s <- services) {
      logger.debug(s"Found service provider ${s.getClass.scalaName}")
      injector.injectMembers(s)
    }
    services
  }

  private val lazyServiceProviderRoute = Lazy[Route] {
    val servicePathRoutes = for (s <- services; (p, r) <- s.pathToRoute) yield (s, p, r)
    logAndCheck(servicePathRoutes)
    combineRoutes(
      for ((_, p, r) <- servicePathRoutes) yield pathSegments(p)(r))
  }

  protected def serviceProviderRoute: Route =
    requestContext => {
      if (lazyServiceProviderRoute.isEmpty) {
        logger.debug(s"Looking up RouteService for unhandled URI ${requestContext.request.uri.path}")
      }
      lazyServiceProviderRoute()(requestContext)
    }
}

private[web] object ServiceProviderRoute
{
  private val logger = Logger(getClass)

  private def logAndCheck(namedRouteToService: Seq[(RouteService, String, Route)]): Unit = {
    if (namedRouteToService.isEmpty) logger.trace(s"No routes")
    else for ((s, p, _) <- namedRouteToService) logger.debug(s"${s.getClass.scalaName} provides route '/$p'")
    for (pathToTriples <- namedRouteToService.duplicateKeys(_._2)) {
      sys.error("Duplicate route paths: " + pathToTriples.values.flatten.map(o => s"'${o._2}'->${o._1.getClass.scalaName}" ).mkString(", "))
    }
  }
}
