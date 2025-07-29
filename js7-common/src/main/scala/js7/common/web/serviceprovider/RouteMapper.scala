package js7.common.web.serviceprovider

import js7.base.auth.Permission
import org.apache.pekko.http.scaladsl.server.Route

trait RouteMapper:
  def pathToRoute(context: RouteServiceContext): Map[String, (Route, Set[Permission])]
