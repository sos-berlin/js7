package com.sos.jobscheduler.master.gui.server

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.{ETag, `Cache-Control`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.gui.server.WebjarsRoute._

/**
  * @author Joacim Zschimmer
  */
trait WebjarsRoute {

  private lazy val webjarsResourceDirectory = JavaResource("META-INF/resources/webjars")

  /** Resource directory for copied (by build.sbt) Webjars files, to avoid distribution of big webjars. */
  protected def copiedWebjarsResource: JavaResource

  protected final val webjarsRoute: Route =
    get {
      respondWithHeader(Caching) {  // WebJar resources are immutable versioned and can be cached forever
        removeEtag(  // WebJar URI contains the version, so only use URI for caching
          getFromResourceDirectory(copiedWebjarsResource.path) ~
          getFromResourceDirectory(webjarsResourceDirectory.path))    // Artifacts of Maven groupId 'org.webjars'
      }
    }
}

private[gui] object WebjarsRoute {
  private val Caching = `Cache-Control`(`max-age`(365*24*3600))

  private def removeEtag: Directive0 =
    mapResponse(r ⇒ r.withHeaders(r.headers filter {
      case ETag(_) ⇒ false
      case _ ⇒ true
    }))
}
