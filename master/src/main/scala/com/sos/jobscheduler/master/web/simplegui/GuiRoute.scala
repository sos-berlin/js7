package com.sos.jobscheduler.master.web.simplegui

import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.MediaTypes.`text/html`
import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.{EntityTag, `Cache-Control`}
import akka.http.scaladsl.model.{DateTime, HttpEntity}
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
  private lazy val IndexHtml = IndexHtmlResource.asUTF8String.replace("{JobSchedulerVersionUuid}", BuildInfo.uuid)

  final def indexHtmlRoute =
    get {
      conditional(EntityTag(BuildInfo.uuid, weak = ETagIsWeak), StartDateTime) {
        respondWithHeader(Caching) {
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
    conditional(EntityTag(BuildInfo.uuid)) {
      respondWithHeader(Caching) {
        getFromResourceDirectory(ResourceDirectory.path)
      }
    }
    extractUnmatchedPath { path ⇒
      logger.warn(s"Not found: .../gui$path (resource ${ResourceDirectory.path}$path)")
      complete(NotFound)
    }
}

object GuiRoute {
  private val logger = Logger(getClass)
  private val ETagIsWeak = BuildInfo.buildVersion.contains("-SNAPSHOT")
  private val MaxAge = if (BuildInfo.version contains "-SNAPSHOT") 30 else 24*3600
  private val Caching = `Cache-Control`(`max-age`(MaxAge))
  private val StartDateTime = DateTime.now
}
