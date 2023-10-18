package js7.controller.web

import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.model.Uri
import org.apache.pekko.http.scaladsl.server.Directives.{complete, extractRequest}

/**
  * @author Joacim Zschimmer
  */
trait RootRoute:

  val root =
    extractRequest { request =>
      complete((NotFound, s"Try ${request.uri.copy(rawQueryString = None).withPath(Uri.Path("/controller/api"))}\n"))
    }
