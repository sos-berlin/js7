package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.DirectAgentApi
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.data.order.{Order, OrderId}
import monix.eval.Task
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait OrderWebService extends AgentRouteProvider {

  protected def agentApi(meta: CommandMeta): DirectAgentApi

  private implicit def implicitScheduler = scheduler

  protected final lazy val orderRoute: Route =
    authorizedUser(ValidUserPermission) { user ⇒
      path(Segment) { orderIdString ⇒
        val orderId = OrderId(orderIdString)
        complete {
          agentApi(CommandMeta(user)).order(orderId): Task[Order[Order.State]]
        }
      } ~
      pathSingleSlash {
        parameter("return" ? "Order") {
          case "OrderId" ⇒
            complete {
              agentApi(CommandMeta(user)).orderIds: Task[Seq[OrderId]]
            }
          case "Order" ⇒
            complete {
              agentApi(CommandMeta(user)).orders: Task[Seq[Order[Order.State]]]
            }
        }
      }
    }
}


