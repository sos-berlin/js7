package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.system.{Stderr, Stdout, StdoutOrStderr}
import com.sos.jobscheduler.data.workflow.{Position, WorkflowId, WorkflowPosition}
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
  sealed trait OrderActorEvent extends OrderCoreEvent

  final case class OrderAdded(workflowId: WorkflowId, state: Idle, payload: Payload = Payload.empty)
  extends OrderCoreEvent {
    workflowId.requireNonAnonymous()
    //type State = Idle
  }

  final case class OrderAttached(workflowPosition: WorkflowPosition, state: Idle, parent: Option[OrderId], agentPath: AgentPath, payload: Payload)
  extends OrderCoreEvent {
    workflowPosition.workflowId.requireNonAnonymous()
    //type State = Idle
  }

  final case class OrderTransferredToAgent(agentPath: AgentPath)
  extends OrderCoreEvent {
    //type State = Idle
  }

  sealed trait OrderTransferredToMaster
  case object OrderTransferredToMaster
  extends OrderCoreEvent {
    //type State = Detached.type
  }

  sealed trait OrderProcessingStarted extends OrderCoreEvent
  case object OrderProcessingStarted extends OrderProcessingStarted {
    //type State = InProcess
  }

  sealed trait OrderStdWritten extends OrderEvent {
    //type State = InProcess

    def stdoutStderrType: StdoutOrStderr
    protected def chunk: String

    override def toString = s"${getClass.simpleScalaName}(${chunk.trim.truncateWithEllipsis(80, showLength = true)})"
  }
  object OrderStdWritten {
    def apply(t: StdoutOrStderr): String ⇒ OrderStdWritten =
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
    //type State = Processed
  }

  final case class OrderForked(children: Seq[OrderForked.Child]) extends OrderActorEvent
  object OrderForked {
    @JsonCodec
    final case class Child(branchId: Position.BranchId.Named, orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty)
  }

  final case class OrderJoined(variablesDiff: MapDiff[String, String], outcome: Outcome)  // TODO Das gleiche wie OrderProcessed ?
  extends OrderActorEvent

  final case class OrderOffered(orderId: OrderId, until: Timestamp)
  extends OrderActorEvent

  final case class OrderAwaiting(orderId: OrderId) extends OrderActorEvent

  final case class OrderMoved(to: Position)
  extends OrderActorEvent {
    //type State = Ready.type
  }

  final case class OrderStopped(outcome: Outcome.NotSucceeded) extends OrderActorEvent

  /**
    * Agent has processed all steps and the Order should be fetched by the Master.
    */
  sealed trait OrderDetachable extends OrderActorEvent
  case object OrderDetachable extends OrderDetachable {
    //type State = Detachable.type
  }

  /**
    * Order has been removed from the Agent and is held by the Master.
    */
  sealed trait OrderDetached extends OrderCoreEvent
  case object OrderDetached extends OrderDetached {
    //type State = Detached.type
  }

  sealed trait OrderFinished extends OrderActorEvent
  case object OrderFinished extends OrderFinished {
    //type State = Finished
  }

  implicit val OrderEventJsonCodec = TypedJsonCodec[OrderEvent](
    Subtype(deriveCodec[OrderAdded]),
    Subtype(deriveCodec[OrderAttached]),
    Subtype(deriveCodec[OrderTransferredToAgent]),
    Subtype(OrderTransferredToMaster),
    Subtype(deriveCodec[OrderStdoutWritten]),
    Subtype(deriveCodec[OrderStderrWritten]),
    Subtype(deriveCodec[OrderProcessed]),
    Subtype(deriveCodec[OrderForked]),
    Subtype(deriveCodec[OrderJoined]),
    Subtype(deriveCodec[OrderOffered]),
    Subtype(deriveCodec[OrderAwaiting]),
    Subtype(OrderDetached),
    Subtype(OrderDetachable),
    Subtype(OrderProcessingStarted),
    Subtype(deriveCodec[OrderMoved]),
    Subtype(deriveCodec[OrderStopped]),
    Subtype(OrderFinished))
}
