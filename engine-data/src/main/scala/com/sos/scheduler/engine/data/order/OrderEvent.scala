package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.Event
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.jobchain.NodeId
import spray.json.DefaultJsonProtocol._

/**
  * Event related to an Order.
  * <p>
  *   See also [[com.sos.scheduler.engine.data.filebased.FileBasedEvent]].
  * <p>
  *   The order's events occur in this order:
  * <ul>
  *   <li>
  *     [[OrderStarted]]
  *   <li>
  *     [[OrderStepStarted]]
  *   <li>
  *     [[OrderStepEnded]]
  *   <li>
  *     [[OrderNodeChanged]]
  *   <li>
  *     [[OrderStepStarted]]
  *   <li>
  *     ...
  *   <li>
  *     [[OrderFinished]]
  * </ul>
  *
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
    Subtype(jsonFormat0(() ⇒ OrderStarted)),
    Subtype(jsonFormat0(() ⇒ OrderWaitingInTask)))
}

/**
  * Order has finished.
  *
  * @param nodeId the `NodeId` of the end node
  */
final case class OrderFinished(nodeId: NodeId)
extends OrderEvent

case object OrderNestedFinished
extends OrderEvent

case object OrderNestedStarted
extends OrderEvent

/**
  * Suspended Order has been resumed.
  */
case object OrderResumed
extends OrderEvent

/**
  * Order has set back.
  *
  * @param nodeId denotes the order's job chain node.
  */
final case class OrderSetBack(nodeId: NodeId)
extends OrderEvent

/**
  * Order has changed its job chain node.
  *
  * @param nodeId denotes the new node.
  * @param fromNodeId denotes the previous node.
  */
final case class OrderNodeChanged(nodeId: NodeId, fromNodeId: NodeId)
extends OrderEvent {
  def nodeIdTransition: (NodeId, NodeId) = fromNodeId → nodeId
}

/**
  * A task has finished processing a step in the order's job chain.
  *
  * @param nodeTransition denotes the type of the result (Success, Error, or Keep).
  */
final case class OrderStepEnded(nodeTransition: OrderNodeTransition)
extends OrderEvent

/**
  * A task has started processing a step in the order's job chain.
  *
  * @param nodeId denotes the job chain node the order is from.
  * @param taskId
  */
final case class OrderStepStarted(nodeId: NodeId, taskId: TaskId)
extends OrderEvent

/**
  * The order has been suspended.
  */
case object OrderSuspended
extends OrderEvent

/**
  * The order has been started.
  * A task will process a first step of the order's job chain.
  */
case object OrderStarted
extends OrderEvent

/**
  * A Task has taken the order and waits for a process (`OrderProcessingState.WaitingInTask`).
  * This event occury only when a task is not immediately available (...).
  */
case object OrderWaitingInTask
extends OrderEvent
