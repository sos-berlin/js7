package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.system.{Stderr, Stdout, StdoutOrStderr}
import com.sos.jobscheduler.data.workflow.WorkflowId
import com.sos.jobscheduler.data.workflow.instructions.Fork
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
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

  final case class OrderAdded(workflowId: WorkflowId, scheduledFor: Option[Timestamp] = None, payload: Payload = Payload.empty)
  extends OrderCoreEvent {
    workflowId.requireNonAnonymous()
    //type State = FreshOrReady
  }
  object OrderAdded {
    private[OrderEvent] implicit val jsonCodec: ObjectEncoder[OrderAdded] =
      o => JsonObject(
        "workflowId" -> o.workflowId.asJson,
        "scheduledFor" -> o.scheduledFor.asJson,
        "variables" -> ((o.payload != Payload.empty) ? o.payload.variables).asJson)

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderAdded] =
      c => for {
        workflowId <- c.get[WorkflowId]("workflowId")
        scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
        payload <- c.get[Option[Map[String, String]]]("variables") map (_ map Payload.apply getOrElse Payload.empty)
      } yield OrderAdded(workflowId, scheduledFor, payload)
  }

  final case class OrderAttached(workflowPosition: WorkflowPosition, state: FreshOrReady, outcome: Outcome,
    parent: Option[OrderId], agentRefPath: AgentRefPath,  payload: Payload)
  extends OrderCoreEvent {
    workflowPosition.workflowId.requireNonAnonymous()
    //type State = FreshOrReady
  }

  final case class OrderTransferredToAgent(agentRefPath: AgentRefPath)
  extends OrderCoreEvent {
    //type State = FreshOrReady
  }

  sealed trait OrderTransferredToMaster
  case object OrderTransferredToMaster
  extends OrderCoreEvent {
    //type State = Detached.type
  }

  sealed trait OrderStarted extends OrderActorEvent
  case object OrderStarted extends OrderStarted

  sealed trait OrderProcessingStarted extends OrderCoreEvent
  case object OrderProcessingStarted extends OrderProcessingStarted {
    //type State = Processing
  }

  sealed trait OrderStdWritten extends OrderEvent {
    //type State = Processing

    def stdoutStderrType: StdoutOrStderr
    protected def chunk: String

    override def toString = getClass.simpleScalaName + "(" +
      chunk.trim.truncateWithEllipsis(80, showLength = true).replace("\n", "\\n").replace("\r", "\\r") + ")"
  }
  object OrderStdWritten {
    def apply(t: StdoutOrStderr): String => OrderStdWritten =
      t match {
        case Stdout => OrderStdoutWritten.apply
        case Stderr => OrderStderrWritten.apply
      }

    def unapply(o: OrderStdWritten) = o match {
      case OrderStdoutWritten(chunk) => Some((Stdout, chunk))
      case OrderStderrWritten(chunk) => Some((Stderr, chunk))
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
    final case class Child(branchId: Fork.Branch.Id, orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty)
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

  /** Only internal. Will be converted to `OrderStopped` or `OrderCatched`. */
  final case class OrderFailed(outcome: Outcome.NotSucceeded) extends OrderActorEvent

  final case class OrderStopped(outcome: Outcome.NotSucceeded) extends OrderActorEvent

  final case class OrderCatched(outcome: Outcome.NotSucceeded, movedTo: Position) extends OrderActorEvent

  final case class OrderRetrying(movedTo: Position, delayedUntil: Option[Timestamp] = None)
  extends OrderActorEvent

  sealed trait OrderAwoke extends OrderActorEvent
  case object OrderAwoke extends OrderAwoke

  final case class OrderBroken(problem: Problem) extends OrderActorEvent

  /**
    * Master may have started to attach Order to Agent..
    */
  final case class OrderAttachable(agentRefPath: AgentRefPath) extends OrderCoreEvent {
    //type State = Attaching.type
  }

  /**
    * Agent has processed all steps and the Order should be fetched by the Master.
    */
  sealed trait OrderDetachable extends OrderActorEvent
  case object OrderDetachable extends OrderDetachable {
    //type State = Detaching.type
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

  /** A OrderCancelationMarked on Agent is different from same Event on Master.
    * Master will ignore the Agent's OrderCancelationMarked.
    * Master should have issued the event independendly. **/
  final case class OrderCancelationMarked(mode: CancelMode) extends OrderActorEvent

  sealed trait OrderCanceled extends OrderActorEvent
  case object OrderCanceled extends OrderCanceled

  implicit val jsonCodec = TypedJsonCodec[OrderEvent](
    Subtype[OrderAdded],
    Subtype(OrderStarted),
    Subtype(OrderProcessingStarted),
    Subtype(deriveCodec[OrderStdoutWritten]),
    Subtype(deriveCodec[OrderStderrWritten]),
    Subtype(deriveCodec[OrderProcessed]),
    Subtype(deriveCodec[OrderStopped]),
    Subtype(deriveCodec[OrderCatched]),
    Subtype(deriveCodec[OrderRetrying]),
    Subtype(OrderAwoke),
    Subtype(deriveCodec[OrderMoved]),
    Subtype(deriveCodec[OrderForked]),
    Subtype(deriveCodec[OrderJoined]),
    Subtype(deriveCodec[OrderOffered]),
    Subtype(deriveCodec[OrderAwaiting]),
    Subtype(OrderFinished),
    Subtype(deriveCodec[OrderCancelationMarked]),
    Subtype(OrderCanceled),
    Subtype(deriveCodec[OrderTransferredToAgent]),
    Subtype(OrderTransferredToMaster),
    Subtype(deriveCodec[OrderAttached]),
    Subtype(deriveCodec[OrderAttachable]),
    Subtype(OrderDetachable),
    Subtype(OrderDetached),
    Subtype(deriveCodec[OrderBroken]))
}
