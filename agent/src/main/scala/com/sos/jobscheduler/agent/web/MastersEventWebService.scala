package com.sos.scheduler.engine.agent.web

import akka.util.Timeout
import com.sos.scheduler.engine.agent.orderprocessing.KeyedEventJsonFormats.keyedEventJsonFormat
import com.sos.scheduler.engine.agent.orderprocessing.OrderHandler
import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.common.event.collector.EventDirectives.eventRequest
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.pathSegments
import com.sos.scheduler.engine.data.engine2.order.OrderEvent
import com.sos.scheduler.engine.data.event.EventRequest
import scala.concurrent.ExecutionContext
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
          }
        }
      }
    }
  }
}
