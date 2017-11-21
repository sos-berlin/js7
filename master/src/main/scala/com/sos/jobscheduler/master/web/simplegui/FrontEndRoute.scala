package com.sos.jobscheduler.master.web.simplegui

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.web.simplegui.FrontEndRoute._

/**
  * @author Joacim Zschimmer
  */
trait FrontEndRoute extends WebjarsRoute {

  // Lazy and not in object to avoid ExceptionInInitializerError
  private lazy val FrontendResourceDirectory = JavaResource("com/sos/jobscheduler/master/web/simplegui/frontend")

  final val frontEndRoute: Route =
    get {
      pathSegments("webjars") {
        webjarsRoute
      } ~
      parameter("SHA-224".?) { hashOption ⇒
        (if (hashOption.isDefined) respondWithHeader(LongTimeCaching) else pass) {
          getFromResourceDirectory(FrontendResourceDirectory.path)
        }
      }
    }
}

object FrontEndRoute {
  private val LongTimeCaching = `Cache-Control`(`max-age`(24*60*60))
}
