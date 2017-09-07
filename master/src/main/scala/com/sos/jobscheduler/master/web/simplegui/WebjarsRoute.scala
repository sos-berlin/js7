package com.sos.jobscheduler.master.web.simplegui

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.html.Webjar
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.web.simplegui.WebjarsRoute._
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
trait WebjarsRoute {

  protected implicit def actorRefFactory: ActorRefFactory
  protected lazy val webjarsExists = checkWebjars(NeededWebjars)

  protected final def webjarsRoute: Route =
    get {
      respondWithHeader(LongTimeCaching) {  // WebJar resources are immutable versioned and can be cached forever
        getFromResourceDirectory(WebjarsResourceDirectory.path)    // Artifacts of Maven groupId 'org.webjars'
      }
    }
}

private[simplegui] object WebjarsRoute {
  private val LongTimeCaching = `Cache-Control`(`max-age`((30 * 24.h).getSeconds))
  private val WebjarsResourceDirectory = JavaResource("META-INF/resources/webjars")
  private val logger = Logger(getClass)

  val NeededWebjars = List(
    new Webjar(basePath = "jquery/2.2.4",
      javascripts = "jquery.min.js" :: Nil),
    new Webjar(basePath = "bootstrap/3.3.6",
      css         = "css/bootstrap.min.css" :: Nil,
      javascripts = "js/bootstrap.min.js" :: Nil))

  private def checkWebjars(webExternals: Iterable[Webjar]): Boolean = {
    var result = true
    for (webExternal ← webExternals; resource ← webExternal.allPaths map WebjarsResourceDirectory./) {
      try resource.url
      catch { case NonFatal(t) ⇒
        logger.debug(s"Missing Webjar JavaResource $resource: $t")
        result = false
      }
    }
    result
  }
}
