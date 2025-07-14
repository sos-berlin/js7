package js7.common.web.serviceprovider

import org.apache.pekko.http.scaladsl.server.Route

trait RouteMapper:
  def pathToRoute(context: RouteServiceContext): Map[String, Route]
