package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.agent.command.{CommandHandler, CommandMeta}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils._
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.data.order.OrderId
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
trait OrderWebService extends AgentWebService {

  protected def commandHandler: CommandHandler
  protected implicit def executionContext: ExecutionContext

  routeBuilder.addApiRoute { user ⇒
    pathSegments("order") {
      path(Segment) { orderIdString ⇒
        val orderId = OrderId(orderIdString)
        complete {
          commandHandler.typedExecute(AgentCommand.GetOrder(orderId), CommandMeta(user)) map { _.order }
        }
      } ~
      pathSingleSlash {
        parameter("return" ? "Order") {
          case "OrderId" ⇒
            complete {
              commandHandler.typedExecute(AgentCommand.GetOrderIds, CommandMeta(user)) map { _.orderIds }
            }
          case "Order" ⇒
            complete {
              commandHandler.typedExecute(AgentCommand.GetOrders, CommandMeta(user)) map { _.orders }
            }
        }
      }
    }
  }
}


