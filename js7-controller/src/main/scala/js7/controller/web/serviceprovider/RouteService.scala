package js7.controller.web.serviceprovider

import akka.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
trait RouteService {
  def pathToRoute: Map[String, Route]
}
