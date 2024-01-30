package js7.controller.web.controller

import org.apache.pekko.http.scaladsl.model.StatusCodes.OK
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
trait TestRoute:
  protected final val testRoute: Route =
    path("post"):
      post:
        complete(OK)
