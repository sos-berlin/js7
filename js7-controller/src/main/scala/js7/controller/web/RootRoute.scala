package js7.controller.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Directives.{complete, extractRequest}

/**
  * @author Joacim Zschimmer
  */
trait RootRoute:

  val root =
    extractRequest { request =>
      complete((NotFound, s"Try ${request.uri.copy(rawQueryString = None).withPath(Uri.Path("/controller/api"))}\n"))
    }
