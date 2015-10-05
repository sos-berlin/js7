package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.NoJobSchedulerEngineService._
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.scalautil.Logger
import spray.http.RemoteAddress
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait NoJobSchedulerEngineService extends ServiceStandards {

  addJobschedulerRoute {
    pathPrefix("engine") {
      (clientIP | provide[RemoteAddress](RemoteAddress.Unknown)) { ip ⇒
        logger.warn(s"To HTTP requestor $ip: $Message")
        complete(404, Message)
      }
    }
  }
}

object NoJobSchedulerEngineService {
  private val logger = Logger(getClass)
  private[web] val Message = "You are trying to access this Universal Agent as a JobScheduler or Classic Agent." +
    " Use this Universal Agent with JobScheduler 1.10 or newer."
}
