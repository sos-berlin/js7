package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.server.Directives._
import akka.util.Timeout
import com.sos.jobscheduler.agent.scheduler.AgentHandle
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.keyedEventJsonCodec
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.data.event.{EventRequest, ReverseEventRequest}
import com.sos.jobscheduler.data.order.OrderEvent
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
trait MastersEventWebService extends AgentWebService {

  protected implicit def executionContext: ExecutionContext
  protected def agentHandle: AgentHandle
  implicit protected def akkaAskTimeout: Timeout

  routeBuilder.addApiRoute { user ⇒
    pathSegments("master") {
      pathSegments("event") {
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
  }
}
