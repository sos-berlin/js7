package com.sos.jobscheduler.master.web.master.gui

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.{EntityTag, `Cache-Control`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.web.master.gui.GuiRoute._

/**
  * @author Joacim Zschimmer
  */
trait GuiRoute extends WebjarsRoute {

  // Lazy and not in object to avoid ExceptionInInitializerError
  private lazy val ResourceDirectory = JavaResource("com/sos/jobscheduler/master/gui/frontend/gui")

  final def indexHtmlRoute =
    get {
      conditional(EntityTag(BuildInfo.buildId)) {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {  // This allows Firefox and Safari (but not Chrome) to use the cache when hitting the back button - for good user experience.
          // A simple page reload must fetch a fresh copy of index.html to get the up-to-date buildId matching the API's buildId
          complete(IndexHtml)
        }
      }
    }

  final val guiRoute: Route =
    get {
      pathSegments("webjars") {
        webjarsRoute
      } ~
      path("index.html") {
        complete(NotFound)
      } ~
      parameter("v".?) { v ⇒
        if (v exists (_ != BuildInfo.buildId))
          complete((NotFound, "Version changed"))
        else
          (if (v.isDefined) respondWithHeader(LongTimeCaching) else /*when browser reads source map*/pass) {
            getFromResourceDirectory(ResourceDirectory.path)
          } ~
          extractUnmatchedPath { path ⇒
            extractUri { uri ⇒
              logger.warn(s"Resource not found: ${ResourceDirectory.path}$path, for path ${uri.path}")
              complete(NotFound)
            }
          }
      }
    }
}

object GuiRoute {
  private val logger = Logger(getClass)
  private val LongTimeCaching = `Cache-Control`(`max-age`(365*24*3600))
}
