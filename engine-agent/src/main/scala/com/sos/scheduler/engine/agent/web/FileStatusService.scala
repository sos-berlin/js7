package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import java.nio.file.{Files, Paths}
import spray.http.CacheDirectives.{`max-age`, `no-cache`, `no-store`}
import spray.http.HttpHeaders.`Cache-Control`
import spray.http.StatusCodes.{NotFound, OK}
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait FileStatusService extends ServiceStandards {

  addApiRoute {
    (path("fileStatus") & get) {
      parameter("file") { path ⇒
        respondWithHeader(`Cache-Control`(`max-age`(0), `no-store`, `no-cache`)) {
          complete {
            val file = Paths.get(path)
            if (Files.exists(file)) OK else NotFound
          }
        }
      }
    }
  }
}
