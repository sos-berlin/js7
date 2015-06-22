package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import java.nio.file.{Files, Paths}
import spray.http.StatusCodes.{NotFound, OK}
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait FileStatusService extends ServiceStandards {

  addRoute {
    (get & pathPrefix("agent" / "fileStatus")) {
      parameter("file") { path ⇒
        complete {
          val file = Paths.get(path)
          if (Files.exists(file)) OK else NotFound
        }
      }
    }
  }
}
