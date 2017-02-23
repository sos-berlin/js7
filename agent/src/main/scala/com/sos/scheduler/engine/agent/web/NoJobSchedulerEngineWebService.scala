package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.NoJobSchedulerEngineWebService._
import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.pathSegments
import spray.http.RemoteAddress
import spray.http.StatusCodes.NotFound
import spray.routing.Directives._


/**
 * @author Joacim Zschimmer
 */
trait NoJobSchedulerEngineWebService extends AgentWebService {

  routeBuilder.addJobschedulerRoute { _ ⇒
    pathSegments("engine") {
      (clientIP | provide[RemoteAddress](RemoteAddress.Unknown)) { ip ⇒
        logger.warn(s"To HTTP requestor $ip: $Message")
        complete((NotFound, Message))
      }
    }
  }
}

object NoJobSchedulerEngineWebService {
  private val logger = Logger(getClass)
  private[web] val Message = "You are trying to access this Universal Agent as a JobScheduler or Classic Agent." +
    " Use this Universal Agent with JobScheduler 1.10 or newer."
}
