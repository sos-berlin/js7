package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.jobnet.{NodeId, NodeKey}
import com.sos.jobscheduler.data.system.StdoutStderr._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event {
  type Key = OrderId
}

object OrderEvent {
  sealed trait OrderCoreEvent extends OrderEvent

  final case class OrderAdded(
    nodeKey: NodeKey,
    state: Order.Idle,
    variables: Map[String, String],
    outcome: Order.Outcome)
  extends OrderCoreEvent

  final case class OrderAttached(
    nodeKey: NodeKey,
    state: Order.Idle,
    variables: Map[String, String],
    outcome: Order.Outcome)
  extends OrderCoreEvent

  final case class OrderMovedToAgent(agentPath: AgentPath)
  extends OrderCoreEvent

  final case object OrderMovedToMaster
  extends OrderCoreEvent

  case object OrderStepStarted extends OrderCoreEvent


  sealed trait OrderStdWritten extends OrderEvent {
    def stdoutStderrType: StdoutStderrType
  }

  object OrderStdWritten {
    def apply(t: StdoutStderrType): String ⇒ OrderStdWritten =
      t match {
        case Stdout ⇒ OrderStdoutWritten
        case Stderr ⇒ OrderStderrWritten
      }

    def unapply(o: OrderStdWritten) = o match {
      case OrderStdoutWritten(chunk) ⇒ Some((Stdout, chunk))
      case OrderStderrWritten(chunk) ⇒ Some((Stderr, chunk))
    }
  }

  final case class OrderStdoutWritten(chunk: String) extends OrderStdWritten {
    def stdoutStderrType = Stdout
  }

  final case class OrderStderrWritten(chunk: String) extends OrderStdWritten {
    def stdoutStderrType = Stderr
  }


  sealed trait OrderStepEnded extends OrderCoreEvent {
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
  case object OrderReady extends OrderCoreEvent

  /**
    * Order has been removed from the Agent and is held by the Master.
    */
  case object OrderDetached extends OrderCoreEvent

  case object OrderFinished extends OrderCoreEvent

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
    Subtype(jsonFormat1(OrderStdoutWritten)),
    Subtype(jsonFormat1(OrderStderrWritten)),
    Subtype(jsonFormat0(() ⇒ OrderDetached)),
    Subtype(jsonFormat0(() ⇒ OrderReady)),
    Subtype(jsonFormat0(() ⇒ OrderStepStarted)),
    Subtype(jsonFormat3(OrderStepSucceeded)),
    Subtype(jsonFormat2(OrderStepFailed)),
    Subtype(jsonFormat0(() ⇒ OrderFinished)))
}
