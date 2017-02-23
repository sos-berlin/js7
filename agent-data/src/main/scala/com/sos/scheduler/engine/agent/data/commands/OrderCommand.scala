package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.agent.data.commandresponses.EmptyResponse
import com.sos.scheduler.engine.data.engine2.order.{JobNet, Order}
import com.sos.scheduler.engine.data.order.OrderId
import scala.collection.immutable.Seq
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderCommand extends Command


final case class AddJobNet(jobNet: JobNet) extends OrderCommand {
  type Response = EmptyResponse.type
}

object AddJobNet {
  val SerialTypeName = "AddJobNet"
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
  final case class Response(order: Order[Order.State]) extends com.sos.scheduler.engine.agent.data.commandresponses.Response
}

case object GetOrderIds extends OrderCommand {
  val SerialTypeName = "GetOrderIds"
  final case class Response(orders: Seq[OrderId]) extends com.sos.scheduler.engine.agent.data.commandresponses.Response
}

case object GetOrders extends OrderCommand {
  val SerialTypeName = "GetOrders"
  final case class Response(order: Seq[Order[Order.State]]) extends com.sos.scheduler.engine.agent.data.commandresponses.Response
}
