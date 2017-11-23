package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.jobnet.{NodeId, NodeKey}
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.system.StdoutStderr._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event {
  type Key = OrderId
  //type State <: Order.State
}

object OrderEvent {
  sealed trait OrderCoreEvent extends OrderEvent

  final case class OrderAdded(
    nodeKey: NodeKey,
    state: Idle,
    variables: Map[String, String],
    outcome: Outcome)
  extends OrderCoreEvent {
    //type State = Idle
  }

  final case class OrderAttached(
    nodeKey: NodeKey,
    state: Idle,
    variables: Map[String, String],
    outcome: Outcome)
  extends OrderCoreEvent {
    //type State = Idle
  }

  final case class OrderMovedToAgent(agentPath: AgentPath)
  extends OrderCoreEvent {
    //type State = Idle
  }

  final case object OrderMovedToMaster
  extends OrderCoreEvent {
    //type State = Detached.type
  }

  case object OrderProcessingStarted extends OrderCoreEvent {
    //type State = InProcess.type
  }


  sealed trait OrderStdWritten extends OrderEvent {
    def stdoutStderrType: StdoutStderrType
    //type State = InProcess.type
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


  final case class OrderProcessed(variablesDiff: MapDiff[String, String], outcome: Outcome) extends OrderCoreEvent {
    //type State = Processed.type
  }

  final case class OrderTransitioned(toNodeId: NodeId)
  extends OrderCoreEvent {
    //type State = Ready.type
  }

  /**
    * Agent has processed all steps and the Order should be fetched by the Master.
    */
  case object OrderDetachable extends OrderCoreEvent {
    //type State = Detachable.type
  }

  /**
    * Order has been removed from the Agent and is held by the Master.
    */
  case object OrderDetached extends OrderCoreEvent {
    //type State = Detached.type
  }

  case object OrderFinished extends OrderCoreEvent {
    //type State = Finished.type
  }

  implicit val OrderEventJsonFormat = TypedJsonFormat[OrderEvent](
    Subtype(jsonFormat4(OrderAdded)),
    Subtype(jsonFormat4(OrderAttached)),
    Subtype(jsonFormat1(OrderMovedToAgent)),
    Subtype(jsonFormat0(() ⇒ OrderMovedToMaster)),
    Subtype(jsonFormat1(OrderStdoutWritten)),
    Subtype(jsonFormat1(OrderStderrWritten)),
    Subtype(jsonFormat2(OrderProcessed)),
    Subtype(jsonFormat0(() ⇒ OrderDetached)),
    Subtype(jsonFormat0(() ⇒ OrderDetachable)),
    Subtype(jsonFormat0(() ⇒ OrderProcessingStarted)),
    Subtype(jsonFormat1(OrderTransitioned)),
    Subtype(jsonFormat0(() ⇒ OrderFinished)))
}
