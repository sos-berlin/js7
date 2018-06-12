package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.command.{CommandHandler, CommandMeta}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.data.order.OrderId
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait OrderWebService extends AgentRouteProvider {

  protected def commandHandler: CommandHandler
  protected implicit def scheduler: Scheduler

  protected final val orderRoute: Route =
    authorizedUser(ValidUserPermission) { user ⇒
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


