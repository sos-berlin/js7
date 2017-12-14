package com.sos.jobscheduler.master.web.master.gui

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.web.master.gui.WebjarsRoute._

/**
  * @author Joacim Zschimmer
  */
trait WebjarsRoute {

  protected implicit def actorRefFactory: ActorRefFactory

  protected final val webjarsRoute: Route =
    get {
      respondWithHeader(Caching) {  // WebJar resources are immutable versioned and can be cached forever
        getFromResourceDirectory(WebjarsResourceDirectory.path)    // Artifacts of Maven groupId 'org.webjars'
      }
    }
}

private[gui] object WebjarsRoute {
  private val Caching = `Cache-Control`(`max-age`(365*24*3600))
  private lazy val WebjarsResourceDirectory = JavaResource("META-INF/resources/webjars")
}
