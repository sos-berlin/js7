package js7.controller.web.serviceprovider

import akka.http.scaladsl.server.Route

trait RouteMapper:
  def pathToRoute(context: RouteServiceContext): Map[String, Route]
