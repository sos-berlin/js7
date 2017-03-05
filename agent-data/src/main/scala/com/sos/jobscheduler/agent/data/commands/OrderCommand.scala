package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.data.jobnet.Jobnet
import com.sos.jobscheduler.data.order.{Order, OrderId}
import scala.collection.immutable.Seq
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderCommand extends Command


final case class AddJobnet(jobnet: Jobnet) extends OrderCommand {
  type Response = EmptyResponse.type
}

object AddJobnet {
  val SerialTypeName = "AddJobnet"
  implicit val jsonFormat = jsonFormat1(apply)
}

final case class AddOrder(order: Order[Order.Idle])
extends OrderCommand {
  type Response = EmptyResponse.type
}

object AddOrder {
  val SerialTypeName = "AddOrder"
  implicit val jsonFormat = jsonFormat1(apply)
}


final case class DetachOrder(orderId: OrderId)
extends OrderCommand {
  type Response = EmptyResponse.type
}

object DetachOrder {
  val SerialTypeName = "DetachOrder"
  implicit val jsonFormat = jsonFormat1(apply)
}


final case class GetOrder(orderId: OrderId) extends OrderCommand {
  val SerialTypeName = "GetOrder"
  type Response = GetOrder.Response
}

object GetOrder {
  final case class Response(order: Order[Order.State]) extends com.sos.jobscheduler.agent.data.commandresponses.Response
}

case object GetOrderIds extends OrderCommand {
  val SerialTypeName = "GetOrderIds"
  final case class Response(orders: Seq[OrderId]) extends com.sos.jobscheduler.agent.data.commandresponses.Response
}

case object GetOrders extends OrderCommand {
  val SerialTypeName = "GetOrders"
  final case class Response(order: Seq[Order[Order.State]]) extends com.sos.jobscheduler.agent.data.commandresponses.Response
}
