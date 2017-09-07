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

  final def frontEndRoute: Route =
    //"Route responses other than HttpResponse or Rejections cannot be cached ": cache(routeCache()) {  // Cache slow Jar reads
      pathSegments("webjars") {
        webjarsRoute
      } ~
      get {
        parameter("SHA-224".?) { hashOption ⇒
          respondWithHeader(if (hashOption.isDefined) LongTimeCaching else ShortTimeCaching) {
            getFromResourceDirectory(FrontendResourceDirectory.path)
          }
        }
      }
    //}
}

object FrontEndRoute {
  /** For quicker response, we assume an installation of a changed JobScheduler version takes more than a minute. */
  private val ShortTimeCaching = `Cache-Control`(`max-age`(60))
  private val LongTimeCaching = `Cache-Control`(`max-age`(24*60*60))
  private val FrontendResourceDirectory = JavaResource("com/sos/jobscheduler/master/web/simplegui/frontend")
}
