package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
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
import io.circe.{Decoder, Encoder, JsonObject}

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event {
  type Key = OrderId
}

object OrderEvent {
  sealed trait OrderCoreEvent extends OrderEvent
  sealed trait OrderActorEvent extends OrderCoreEvent
  sealed trait OrderTerminated extends OrderEvent

  final case class OrderAdded(workflowId: WorkflowId, scheduledFor: Option[Timestamp] = None, arguments: Map[String, String] = Map.empty)
  extends OrderCoreEvent {
    workflowId.requireNonAnonymous()
  }
  object OrderAdded {
    private[OrderEvent] implicit val jsonCodec: Encoder.AsObject[OrderAdded] =
      o => JsonObject(
        "workflowId" -> o.workflowId.asJson,
        "scheduledFor" -> o.scheduledFor.asJson,
        "arguments" -> (o.arguments.nonEmpty ? o.arguments).asJson)

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderAdded] =
      c => for {
        workflowId <- c.get[WorkflowId]("workflowId")
        scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
        arguments <- c.get[Option[Map[String, String]]]("arguments") map (_ getOrElse Map.empty)
      } yield OrderAdded(workflowId, scheduledFor, arguments)
  }

  final case class OrderAttached(arguments: Map[String, String], workflowPosition: WorkflowPosition, state: IsFreshOrReady, historicOutcomes: Seq[HistoricOutcome],
    parent: Option[OrderId], agentRefPath: AgentRefPath)
  extends OrderCoreEvent {
    workflowPosition.workflowId.requireNonAnonymous()
  }

  final case class OrderTransferredToAgent(agentRefPath: AgentRefPath)
  extends OrderCoreEvent

  type OrderTransferredToMaster = OrderTransferredToMaster.type
  case object OrderTransferredToMaster
  extends OrderCoreEvent

  type OrderStarted = OrderStarted.type
  case object OrderStarted extends OrderActorEvent

  type OrderProcessingStarted = OrderProcessingStarted.type
  case object OrderProcessingStarted extends OrderCoreEvent

  sealed trait OrderStdWritten extends OrderEvent {

    def stdoutStderrType: StdoutOrStderr
    protected def chunk: String

    override def toString = getClass.simpleScalaName + "(" +
      chunk.truncateWithEllipsis(80, showLength = true).replace("\n", "\\n").replace("\r", "\\r") + ")"
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

  final case class OrderProcessed(outcome: Outcome) extends OrderCoreEvent

  final case class OrderForked(children: Seq[OrderForked.Child]) extends OrderActorEvent
  object OrderForked {
    @JsonCodec
    final case class Child(branchId: Fork.Branch.Id, orderId: OrderId)
  }

  final case class OrderJoined(outcome: Outcome)
  extends OrderActorEvent

  final case class OrderOffered(orderId: OrderId, until: Timestamp)
  extends OrderActorEvent

  final case class OrderAwaiting(orderId: OrderId) extends OrderActorEvent

  final case class OrderMoved(to: Position)
  extends OrderActorEvent

  // TODO OrderCatched should not contain key-values ?
  final case class OrderFailed(outcome: Outcome.NotSucceeded)
  extends OrderActorEvent

  final case class OrderFailedInFork(outcome: Outcome.NotSucceeded)
  extends OrderActorEvent

  /** Only internal. Will be converted to `OrderFailed` or `OrderCatched`. */
  final case class OrderFailedCatchable(outcome: Outcome.NotSucceeded)
  extends OrderActorEvent

  // TODO OrderCatched should not contain key-values
  final case class OrderCatched(outcome: Outcome.NotSucceeded, movedTo: Position) extends OrderActorEvent

  final case class OrderRetrying(movedTo: Position, delayedUntil: Option[Timestamp] = None)
  extends OrderActorEvent

  type OrderAwoke = OrderAwoke.type
  case object OrderAwoke extends OrderActorEvent

  final case class OrderBroken(problem: Problem) extends OrderActorEvent

  /**
    * Master may have started to attach Order to Agent..
    */
  final case class OrderAttachable(agentRefPath: AgentRefPath) extends OrderCoreEvent

  type OrderDetachable = OrderDetachable.type
  /**
    * Agent has processed all steps and the Order should be fetched by the Master.
    */
  case object OrderDetachable extends OrderActorEvent

  type OrderDetached = OrderDetached.type
  /**
    * Order has been removed from the Agent and is held by the Master.
    */
  case object OrderDetached extends OrderCoreEvent

  type OrderFinished = OrderFinished.type
  case object OrderFinished extends OrderActorEvent with OrderTerminated

  /** A OrderCancellationMarked on Agent is different from same Event on Master.
    * Master will ignore the Agent's OrderCancellationMarked.
    * Master should have emitted the event independendly. **/
  final case class OrderCancellationMarked(mode: CancelMode) extends OrderActorEvent

  type OrderCancelled = OrderCancelled.type
  case object OrderCancelled extends OrderActorEvent with OrderTerminated

  implicit val jsonCodec = TypedJsonCodec[OrderEvent](
    Subtype[OrderAdded],
    Subtype(OrderStarted),
    Subtype(OrderProcessingStarted),
    Subtype(deriveCodec[OrderStdoutWritten]),
    Subtype(deriveCodec[OrderStderrWritten]),
    Subtype(deriveCodec[OrderProcessed]),
    Subtype(deriveCodec[OrderCatched]),
    Subtype(deriveCodec[OrderRetrying]),
    Subtype(OrderAwoke),
    Subtype(deriveCodec[OrderMoved]),
    Subtype(deriveCodec[OrderForked]),
    Subtype(deriveCodec[OrderJoined]),
    Subtype(deriveCodec[OrderOffered]),
    Subtype(deriveCodec[OrderAwaiting]),
    Subtype(OrderFinished),
    Subtype(deriveCodec[OrderFailed]),
    Subtype(deriveCodec[OrderFailedInFork]),
    Subtype(deriveCodec[OrderCancellationMarked]),
    Subtype(OrderCancelled),
    Subtype(deriveCodec[OrderTransferredToAgent]),
    Subtype(OrderTransferredToMaster),
    Subtype(deriveCodec[OrderAttached]),
    Subtype(deriveCodec[OrderAttachable]),
    Subtype(OrderDetachable),
    Subtype(OrderDetached),
    Subtype(deriveCodec[OrderBroken]))
}
