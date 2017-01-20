package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.common.sprayutils.SimpleTypeSprayJsonSupport._
import java.nio.file.Files.exists
import java.nio.file.Paths
import spray.http.CacheDirectives.{`max-age`, `no-cache`, `no-store`}
import spray.http.HttpHeaders.`Cache-Control`
import spray.json.DefaultJsonProtocol._
import spray.json.JsBoolean
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait FileStatusWebService extends AgentWebService {

  routeBuilder.addApiRoute {
    (path("fileExists") & get) {
      parameter("file") { path ⇒
        respondWithHeader(`Cache-Control`(`max-age`(0), `no-store`, `no-cache`)) {
          complete {
            JsBoolean(exists(Paths.get(path)))
          }
        }
      }
    }
  }
}
