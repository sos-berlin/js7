package com.sos.jobscheduler.master.gui.server

import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.{EntityTag, `Cache-Control`, `Last-Modified`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.StandardDirectives._
import com.sos.jobscheduler.common.akkahttp.html.HtmlDirectives.pathEndElseRedirect
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.gui.server.GuiRoute._
import com.typesafe.config.Config

/**
  * @author Joacim Zschimmer
  */
trait GuiRoute extends WebjarsRoute {

  // Lazy and not in object to avoid ExceptionInInitializerError
  private lazy val ResourceDirectory = JavaResource("com/sos/jobscheduler/master/gui/browser/gui")
  protected def copiedWebjarsResource = ResourceDirectory / "webjars"
  protected def config: Config

  private val lastModifiedWhenStarted = `Last-Modified`(DateTime.now)
  private lazy val indexHtmlHttpEntity = new IndexHtml(config).toHttpEntity(`UTF-8`)

  final lazy val indexHtmlRoute =
    pathEndElseRedirect {
      get {
        // A simple page reload must fetch a fresh copy of index.html to get the up-to-date buildId matching the API's buildId
        conditional(EntityTag(BuildInfo.buildId)) {
          conditional(lastModifiedWhenStarted.date) {
            respondWithHeader(lastModifiedWhenStarted) {
              // "Max-Age: 0" allows Firefox and Safari (but not Chrome) to use the cache when hitting the back button,
              // showing the same page (with kept JavaScript state) - for good user experience.
              respondWithHeader(`Cache-Control`(`max-age`(0))) {
                complete(indexHtmlHttpEntity)
              }
            }
          }
        }
      }
    }

  final lazy val guiRoute: Route = {
    get {
      pathPrefix(Segment) {
        case "webjars" ⇒
          webjarsRoute

        case VersionSegment ⇒
          immutableResource {
            getFromResourceDirectory(ResourceDirectory.path)
          } ~
          extractUnmatchedPath { path ⇒
            extractUri { uri ⇒
              logger.debug(s"Resource not found: ${ResourceDirectory.path}$path, for path ${uri.path}")
              complete(NotFound)
            }
          }

        case o if o startsWith "v=" ⇒
          complete(NotFound → "Version changed")

        case _ ⇒ complete(NotFound)
      } ~
      path("index.html") {
        complete(NotFound)
      }
    }
  }
}

object GuiRoute {
  private val VersionSegment = "v=" + BuildInfo.buildId
  private val logger = Logger(getClass)
}
