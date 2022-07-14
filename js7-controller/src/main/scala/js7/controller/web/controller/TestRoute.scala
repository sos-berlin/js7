package js7.controller.web.controller

import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
trait TestRoute
{
  protected final val testRoute: Route =
    path("post") {
      post {
        complete(OK)
      }
    }
}
