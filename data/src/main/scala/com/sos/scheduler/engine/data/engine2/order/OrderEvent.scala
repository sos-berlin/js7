package com.sos.scheduler.engine.data.engine2.order

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.base.utils.MapDiff
import com.sos.scheduler.engine.data.engine2.agent.AgentPath
import com.sos.scheduler.engine.data.engine2.order.Order.{Idle, Outcome, State}
import com.sos.scheduler.engine.data.event.Event
import com.sos.scheduler.engine.data.order.OrderId
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event {
  type Key = OrderId
}

object OrderEvent {
  final case class OrderAdded(
    nodeKey: NodeKey,
    state: Idle,
    variables: Map[String, String],
    outcome: Outcome)
  extends OrderEvent

  final case class OrderAttached(
    nodeKey: NodeKey,
    state: Idle,
    variables: Map[String, String],
    outcome: Outcome)
  extends OrderEvent

  final case class OrderMovedToAgent(agentPath: AgentPath)
  extends OrderEvent

  final case object OrderMovedToMaster
  extends OrderEvent

  case object OrderStepStarted extends OrderEvent

  sealed trait OrderStepEnded extends OrderEvent

  final case class OrderStepSucceeded(
    variablesDiff: MapDiff[String, String],
    returnValue: Boolean)
  extends OrderStepEnded

  final case class OrderStepFailed(error: String)
  extends OrderStepEnded

  final case class OrderNodeChanged(nodeId: NodeId) extends OrderEvent

  /**
    * Agent has processed all steps and the Order should be fetched by the Master.
    */
  case object OrderReady extends OrderEvent

  /**
    * Order has been removed from the Agent and is held by the Master.
    */
  case object OrderDetached extends OrderEvent

  case object OrderFinished extends OrderEvent

  //final case class Scheduled(at: Option[Instant]) extends Event
  //case object Started extends Event
  //final case class StepPostponed(variables: Map[String, String], until: Instant) extends StepProcessed
  //final case class NodeChanged(nodeId: NodeId) extends Event
  //case object Removed extends Event

  implicit val OrderEventJsonFormat = TypedJsonFormat[OrderEvent](
    Subtype(jsonFormat4(OrderAdded)),
    Subtype(jsonFormat4(OrderAttached)),
    Subtype(jsonFormat1(OrderMovedToAgent)),
    Subtype(jsonFormat0(() ⇒ OrderMovedToMaster)),
    Subtype(jsonFormat0(() ⇒ OrderDetached)),
    Subtype(jsonFormat0(() ⇒ OrderReady)),
    Subtype(jsonFormat0(() ⇒ OrderStepStarted)),
    Subtype(jsonFormat2(OrderStepSucceeded)),
    Subtype(jsonFormat1(OrderStepFailed)),
    Subtype(jsonFormat1(OrderNodeChanged)),
    Subtype(jsonFormat0(() ⇒ OrderFinished)))
}
