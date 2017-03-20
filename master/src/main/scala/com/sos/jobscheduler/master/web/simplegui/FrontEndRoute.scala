package com.sos.jobscheduler.master.web.simplegui

import com.sos.jobscheduler.common.sprayutils.SprayUtils.pathSegments
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.web.simplegui.FrontEndRoute._
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.routing.Directives._
import spray.routing.Route

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
