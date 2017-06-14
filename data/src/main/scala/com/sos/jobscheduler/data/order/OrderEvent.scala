package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.jobnet.{NodeId, NodeKey}
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
    state: Order.Idle,
    variables: Map[String, String],
    outcome: Order.Outcome)
  extends OrderEvent

  final case class OrderAttached(
    nodeKey: NodeKey,
    state: Order.Idle,
    variables: Map[String, String],
    outcome: Order.Outcome)
  extends OrderEvent

  final case class OrderMovedToAgent(agentPath: AgentPath)
  extends OrderEvent

  final case object OrderMovedToMaster
  extends OrderEvent

  case object OrderStepStarted extends OrderEvent

  sealed trait OrderStepEnded extends OrderEvent {
    def nextNodeId: NodeId
  }

  final case class OrderStepSucceeded(
    variablesDiff: MapDiff[String, String],
    returnValue: Boolean,
    nextNodeId: NodeId)
  extends OrderStepEnded

  final case class OrderStepFailed(error: String, nextNodeId: NodeId)
  extends OrderStepEnded

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
    Subtype(jsonFormat3(OrderStepSucceeded)),
    Subtype(jsonFormat2(OrderStepFailed)),
    Subtype(jsonFormat0(() ⇒ OrderFinished)))
}
