package js7.controller.web.serviceprovider

import org.apache.pekko.http.scaladsl.server.Route

/** A Java service providing more routes for the Controller web services. */
trait RouteService
{
  def pathToRoute(context: RouteServiceContext): Map[String, Route]
}
