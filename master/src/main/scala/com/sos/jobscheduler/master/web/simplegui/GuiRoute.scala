package com.sos.jobscheduler.master.web.simplegui

import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`text/html`
import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.{EntityTag, `Cache-Control`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.web.simplegui.GuiRoute._

/**
  * @author Joacim Zschimmer
  */
trait GuiRoute extends WebjarsRoute {

  // Lazy and not in object to avoid ExceptionInInitializerError
  private lazy val ResourceDirectory = JavaResource("com/sos/jobscheduler/master/gui/frontend/gui")
  private lazy val IndexHtmlResource = JavaResource("com/sos/jobscheduler/master/gui/frontend/gui/index.html")
  private lazy val IndexHtml = IndexHtmlResource.asUTF8String.replace("{JobSchedulerBuildId}", BuildInfo.buildId)

  final def indexHtmlRoute =
    get {
      conditional(EntityTag(BuildInfo.buildId)) {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {  // This allows Firefox and Safari (but not Chrome) to use the cache when hitting the back button - for good user experience.
          // A simple page reload must fetch a fresh copy of index.html to get the up-to-date buildId matching the API's buildId
          complete {
            HttpEntity(`text/html` withCharset `UTF-8`, IndexHtml)
          }
        }
      }
    }

  final val guiRoute: Route =
    path("index.html") {
      complete(NotFound)
    } ~
    parameter("v".?) { v ⇒
      if (v exists (_ != BuildInfo.buildId))
        complete((NotFound, "Version changed"))
      else
        (if (v.isDefined) respondWithHeader(Caching) else /*when browser reads source map*/pass) {
          getFromResourceDirectory(ResourceDirectory.path)
        } ~
        extractUnmatchedPath { path ⇒
          logger.warn(s"Not found: .../gui$path (resource ${ResourceDirectory.path}$path)")
          complete(NotFound)
        }
    }
}

object GuiRoute {
  private val logger = Logger(getClass)
  private val Caching = `Cache-Control`(`max-age`(30*24*3600))
}
