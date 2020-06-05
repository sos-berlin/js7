package js7.master.web.master

import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.server.Directives._
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
