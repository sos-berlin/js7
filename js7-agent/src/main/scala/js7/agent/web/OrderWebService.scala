package js7.agent.web

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.agent.DirectAgentApi
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonSupport._
import js7.core.command.CommandMeta
import js7.data.order.{Order, OrderId}
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait OrderWebService extends AgentRouteProvider {

  protected def agentApi(meta: CommandMeta): DirectAgentApi

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final lazy val orderRoute: Route =
    authorizedUser(ValidUserPermission) { user =>
      path(Segment) { orderIdString =>
        val orderId = OrderId(orderIdString)
        completeTask[Checked[Order[Order.State]]](
          agentApi(CommandMeta(user)).order(orderId))
      } ~
      pathSingleSlash {
        parameter("return" ? "Order") {
          case "OrderId" =>
            completeTask[Checked[Seq[OrderId]]](
              agentApi(CommandMeta(user)).orderIds)
          case "Order" =>
            completeTask[Checked[Seq[Order[Order.State]]]](
              agentApi(CommandMeta(user)).orders)
        }
      }
    }
}
