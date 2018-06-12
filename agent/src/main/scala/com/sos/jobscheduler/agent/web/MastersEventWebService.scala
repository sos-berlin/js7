package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.keyedEventJsonCodec
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.{EventRequest, ReverseEventRequest}
import com.sos.jobscheduler.data.order.OrderEvent
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait MastersEventWebService extends AgentRouteProvider {

  protected implicit def scheduler: Scheduler
  protected def agentHandle: AgentHandle
  implicit protected def akkaAskTimeout: Timeout

  protected final val masterEventRoute: Route =
    authorizedUser(ValidUserPermission) { user ⇒
      pathEnd {
        eventRequest[OrderEvent](defaultReturnType = Some("OrderEvent")).apply {
          case _: ReverseEventRequest[OrderEvent] ⇒
            complete(Problem("ReverseEventRequest is not supported here"))

          case request: EventRequest[OrderEvent] ⇒
            complete(agentHandle.fetchEvents(user.id, request))
        }
      }
    }
}
