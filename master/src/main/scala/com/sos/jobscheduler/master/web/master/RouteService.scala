package com.sos.jobscheduler.master.web.master

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.StandardDirectives.lazyRoute
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.master.web.master.RouteService.NamedRoute
import java.util.ServiceLoader
import monix.eval.Coeval
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext

/**
  * Java service provider interface.
  * @author Joacim Zschimmer
  */
trait RouteService {
  def namedRoutes: Seq[NamedRoute]
}

object RouteService {
  private val logger = Logger(getClass)
  private val InterfaceName = classOf[RouteService].scalaName

  private val routeServices = Coeval.evalOnce {
    val routes: Seq[NamedRouteService] = ServiceLoader.load(classOf[RouteService]).iterator.asScala
      .flatMap(svc ⇒ svc.namedRoutes map (r ⇒ NamedRouteService(r, svc))).toVector
    routes.duplicateKeys(_.namedRoute.suburi) match {
      case duplicates if duplicates.nonEmpty ⇒ sys.error("Duplicate RouteService: " + duplicates.values.flatten.map(_.toString).mkString(", "))
      case _ ⇒
    }
    if (routes.isEmpty) logger.debug(s"No service provider for $InterfaceName")
    else for (r ← routes) logger.debug(s"Service provider for $InterfaceName: $r")
    routes
  }

  private case class NamedRouteService(namedRoute: NamedRoute, service: RouteService) {
    override def toString = '"' + (if (namedRoute.suburi.isEmpty) "" else "/" + namedRoute.suburi) +
      "\"->" + service.getClass.scalaName
  }

  final case class NamedRoute(suburi: String, route: Route)

  private[master] trait RouteServiceRoute {
    protected implicit def executionContext: ExecutionContext

    private val _route: Coeval[Route] = routeServices.map(
      _.map {
        case NamedRouteService(NamedRoute("", route), _) ⇒ route
        case NamedRouteService(NamedRoute(name, route), _) ⇒ pathSegments(name)(route)
      }.foldFast(reject)(_ ~ _))

    final def routeServiceRoute: Route = lazyRoute(_route.value)
  }
}


