package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.base.utils.Strings.TruncatedString
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.system.StdoutStderr._
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath, WorkflowPosition}
import io.circe.generic.JsonCodec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event {
  type Key = OrderId
  //type State <: Order.State
}

object OrderEvent {
  sealed trait OrderCoreEvent extends OrderEvent
  sealed trait OrderTransitionedEvent extends OrderCoreEvent

  final case class OrderAdded(workflowPath: WorkflowPath, state: Idle, payload: Payload = Payload.empty)
  extends OrderCoreEvent {
    //type State = Idle
  }

  final case class OrderAttached(workflowPosition: WorkflowPosition, state: Idle, parent: Option[OrderId], agentPath: AgentPath, payload: Payload)
  extends OrderCoreEvent {
    //type State = Idle
  }

  final case class OrderTransferredToAgent(agentPath: AgentPath)
  extends OrderCoreEvent {
    //type State = Idle
  }

  final case object OrderTransferredToMaster
  extends OrderCoreEvent {
    //type State = Detached.type
  }

  case object OrderProcessingStarted extends OrderCoreEvent {
    //type State = InProcess.type
  }

  sealed trait OrderStdWritten extends OrderEvent {
    //type State = InProcess.type

    def stdoutStderrType: StdoutStderrType
    protected def chunk: String

    override def toString = s"${getClass.simpleScalaName}(${chunk.trim.truncateWithEllipsis(80, showLength = true)})"
  }
  object OrderStdWritten {
    def apply(t: StdoutStderrType): String ⇒ OrderStdWritten =
      t match {
        case Stdout ⇒ OrderStdoutWritten.apply
        case Stderr ⇒ OrderStderrWritten.apply
      }

    def unapply(o: OrderStdWritten) = o match {
      case OrderStdoutWritten(chunk) ⇒ Some((Stdout, chunk))
      case OrderStderrWritten(chunk) ⇒ Some((Stderr, chunk))
    }
  }

  final case class OrderStdoutWritten(chunk: String) extends OrderStdWritten {
    def stdoutStderrType = Stdout
    override def toString = super.toString
  }

  final case class OrderStderrWritten(chunk: String) extends OrderStdWritten {
    def stdoutStderrType = Stderr
    override def toString = super.toString
  }

  final case class OrderProcessed(variablesDiff: MapDiff[String, String], outcome: Outcome) extends OrderCoreEvent {
    //type State = Processed.type
  }

  final case class OrderForked(children: Seq[OrderForked.Child]) extends OrderTransitionedEvent
  object OrderForked {
    @JsonCodec
    final case class Child(childId: OrderId.ChildId, orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty)
  }

  final case class OrderJoined(to: Position, variablesDiff: MapDiff[String, String], outcome: Outcome)
  extends OrderTransitionedEvent

  final case class OrderMoved(to: Position)
  extends OrderTransitionedEvent {
    //type State = Ready.type
  }

  /**
    * Agent has processed all steps and the Order should be fetched by the Master.
    */
  case object OrderDetachable extends OrderTransitionedEvent {
    //type State = Detachable.type
  }

  /**
    * Order has been removed from the Agent and is held by the Master.
    */
  case object OrderDetached extends OrderCoreEvent {
    //type State = Detached.type
  }

  case object OrderFinished extends OrderTransitionedEvent {
    //type State = Finished.type
  }

  implicit val OrderEventJsonCodec = TypedJsonCodec[OrderEvent](
    Subtype(deriveCirceCodec[OrderAdded]),
    Subtype(deriveCirceCodec[OrderAttached]),
    Subtype(deriveCirceCodec[OrderTransferredToAgent]),
    Subtype(OrderTransferredToMaster),
    Subtype(deriveCirceCodec[OrderStdoutWritten]),
    Subtype(deriveCirceCodec[OrderStderrWritten]),
    Subtype(deriveCirceCodec[OrderProcessed]),
    Subtype(deriveCirceCodec[OrderForked]),
    Subtype(deriveCirceCodec[OrderJoined]),
    Subtype(OrderDetached),
    Subtype(OrderDetachable),
    Subtype(OrderProcessingStarted),
    Subtype(deriveCirceCodec[OrderMoved]),
    Subtype(OrderFinished))
}
