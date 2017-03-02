package com.sos.jobscheduler.agent.web

import com.sos.jobscheduler.agent.data.commands.{GetOrder, GetOrderIds, GetOrders}
import com.sos.jobscheduler.agent.scheduler.OrderHandler
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.sprayutils.SprayUtils._
import com.sos.jobscheduler.data.order.OrderId
import scala.concurrent.ExecutionContext
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._

/**
  * @author Joacim Zschimmer
  */
trait OrderWebService extends AgentWebService {

  protected def orderHandler: OrderHandler
  protected def eventIdGenerator: EventIdGenerator
  protected implicit def executionContext: ExecutionContext

  routeBuilder.addApiRoute { user ⇒
    pathSegments("order") {
      path(Segment) { orderIdString ⇒
        val orderId = OrderId(orderIdString)
        complete {
          orderHandler.execute(user.id, GetOrder(orderId)) map { _.order }
        }
      } ~
      pathSingleSlash {
        parameter("return" ? "Order") {
          case "OrderId" ⇒
            complete {
              orderHandler.execute(user.id, GetOrderIds) map { _.orders }
            }
          case "Order" ⇒
            complete {
              orderHandler.execute(user.id, GetOrders) map { _.order }
            }
        }
      }
    }
  }

  //private def eventRoute[E <: Event: KeyedTypedEventJsonFormat]: Route =
  //  pathEnd {
  //    eventRequest[Event](defaultReturnType = Some("Event")).apply { request ⇒
  //      complete {
  //        eventIdGenerator.stampEventSeq {
  //          eventCollector.byPredicate(request, (e: AnyKeyedEvent) ⇒
  //            implicitly[KeyedTypedEventJsonFormat[E]].canSerialize(e.event))
  //        }
  //      }
  //    }
  //  }
}


