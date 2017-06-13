package com.sos.jobscheduler.agent.web

import akka.util.Timeout
import com.sos.jobscheduler.agent.scheduler.OrderHandler
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.keyedEventJsonFormat
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.common.event.collector.EventDirectives.eventRequest
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.sprayutils.SprayUtils.pathSegments
import com.sos.jobscheduler.data.event.{EventRequest, ReverseEventRequest}
import com.sos.jobscheduler.data.order.OrderEvent
import scala.concurrent.ExecutionContext
import spray.http.StatusCodes.BadRequest
import spray.routing.Directives._

/**
  * @author Joacim Zschimmer
  */
trait MastersEventWebService extends AgentWebService {

  protected implicit def executionContext: ExecutionContext
  protected def orderHandler: OrderHandler
  implicit protected def akkaAskTimeout: Timeout

  routeBuilder.addApiRoute { user ⇒
    pathSegments("master") {
      pathSegments("event") {
        pathEnd {
          eventRequest[OrderEvent](defaultReturnType = Some("OrderEvent")).apply {
            case request: EventRequest[OrderEvent] ⇒
              complete {
                for (events ← orderHandler.events(user.id, request)) yield
                  events
              }
            case _: ReverseEventRequest[OrderEvent] ⇒
              complete(BadRequest, "ReverseEventRequest is not supported here")
          }
        }
      }
    }
  }
}
