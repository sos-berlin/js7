package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.system.StdoutStderr._
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey}

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event {
  type Key = OrderId
  //type State <: Order.State
}

object OrderEvent {
  sealed trait OrderCoreEvent extends OrderEvent

  final case class OrderAdded(nodeKey: NodeKey, state: Idle, payload: Payload)
  extends OrderCoreEvent {
    //type State = Idle
  }

  final case class OrderAttached(nodeKey: NodeKey, state: Idle, agentPath: AgentPath, payload: Payload)
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

  implicit val OrderEventJsonCodec = TypedJsonCodec[OrderEvent](
    Subtype(deriveCirceCodec[OrderAdded]),
    Subtype(deriveCirceCodec[OrderAttached]),
    Subtype(deriveCirceCodec[OrderMovedToAgent]),
    Subtype(OrderMovedToMaster),
    Subtype(deriveCirceCodec[OrderStdoutWritten]),
    Subtype(deriveCirceCodec[OrderStderrWritten]),
    Subtype(deriveCirceCodec[OrderProcessed]),
    Subtype(OrderDetached),
    Subtype(OrderDetachable),
    Subtype(OrderProcessingStarted),
    Subtype(deriveCirceCodec[OrderTransitioned]),
    Subtype(OrderFinished))
}
