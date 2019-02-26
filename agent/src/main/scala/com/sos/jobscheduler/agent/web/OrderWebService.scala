package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.DirectAgentApi
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.implicits.checkedJsonEncoder
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.data.order.{Order, OrderId}
import monix.execution.Scheduler
import scala.collection.immutable.Seq

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
