package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.Event
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.jobchain.NodeId
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event {
  type Key = OrderKey
}

object OrderEvent {
  implicit val OrderEventJsonFormat = TypedJsonFormat[OrderEvent](
    Subtype(jsonFormat1(OrderFinished)),
    Subtype(jsonFormat0(() ⇒ OrderNestedFinished)),
    Subtype(jsonFormat0(() ⇒ OrderNestedStarted)),
    Subtype(jsonFormat0(() ⇒ OrderResumed)),
    Subtype(jsonFormat1(OrderSetBack)),
    Subtype(jsonFormat2(OrderNodeChanged)),
    Subtype(jsonFormat1(OrderStepEnded)),
    Subtype(jsonFormat2(OrderStepStarted)),
    Subtype(jsonFormat0(() ⇒ OrderSuspended)),
    Subtype(jsonFormat0(() ⇒ OrderStarted)))
}

final case class OrderFinished(nodeId: NodeId)
extends OrderEvent

case object OrderNestedFinished
extends OrderEvent

case object OrderNestedStarted
extends OrderEvent

case object OrderResumed
extends OrderEvent

final case class OrderSetBack(nodeId: NodeId)
extends OrderEvent

final case class OrderNodeChanged(nodeId: NodeId, fromNodeId: NodeId)
extends OrderEvent {
  def nodeIdTransition: (NodeId, NodeId) = fromNodeId → nodeId
}

final case class OrderStepEnded(nodeTransition: OrderNodeTransition)
extends OrderEvent

final case class OrderStepStarted(nodeId: NodeId, taskId: TaskId)
extends OrderEvent

case object OrderSuspended
extends OrderEvent

case object OrderStarted
extends OrderEvent
