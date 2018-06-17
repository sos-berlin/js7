package com.sos.jobscheduler.master.web.serviceprovider

import akka.http.scaladsl.server.Directives.{reject, _}
import akka.http.scaladsl.server.Route
import com.google.inject.Injector
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.StandardDirectives.lazyRoute
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.master.web.serviceprovider.ServiceProviderRoute._
import java.util.ServiceLoader
import monix.eval.Coeval
import monix.execution.Scheduler
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
private[web] trait ServiceProviderRoute {
  protected implicit def scheduler: Scheduler
  protected implicit def injector: Injector

  private lazy val _route: Coeval[Route] = routeServices(injector).map(
    _.map {
      case NamedRouteService(NamedRoute("", route), _) ⇒ route
      case NamedRouteService(NamedRoute(name, route), _) ⇒ pathSegments(name)(route)
    }.foldFast(reject)(_ ~ _))

  final def serviceProviderRoute: Route = lazyRoute(_route.value)
}

private[web] object ServiceProviderRoute {
  private val logger = Logger(getClass)
  private val InterfaceName = classOf[RouteService].scalaName

  private def routeServices(injector: Injector): Coeval[Seq[NamedRouteService]] =
    Coeval.evalOnce {
      val routes: Seq[NamedRouteService] = ServiceLoader.load(classOf[RouteService]).iterator.asScala
        .flatMap { svc ⇒
          injector.injectMembers(svc)
          svc.namedRoutes map (r ⇒ NamedRouteService(r, svc))
        }.toVector
      routes.duplicateKeys(_.namedRoute.suburi) match {
        case duplicates if duplicates.nonEmpty ⇒ sys.error("Duplicate RouteService: " + duplicates.values.flatten.map(_.toString).mkString(", "))
        case _ ⇒
      }
      if (routes.isEmpty) logger.trace(s"No service provider for $InterfaceName")
      else for (r ← routes) logger.debug(s"Service provider for $InterfaceName: $r")
      routes
    }

  private case class NamedRouteService(namedRoute: NamedRoute, service: RouteService) {
    override def toString = '"' + (if (namedRoute.suburi.isEmpty) "" else "/" + namedRoute.suburi) +
      "\"->" + service.getClass.scalaName
  }
}
