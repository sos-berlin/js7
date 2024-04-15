package js7.controller.web

import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.model.Uri
import org.apache.pekko.http.scaladsl.server.Directives.{complete, extractRequest}
import org.apache.pekko.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
trait RootRoute:

  val root: Route =
    extractRequest { request =>
      complete((NotFound, s"Try ${request.uri.copy(rawQueryString = None).withPath(Uri.Path("/controller/api"))}\n"))
    }
