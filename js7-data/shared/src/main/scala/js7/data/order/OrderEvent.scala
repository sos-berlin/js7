package js7.data.order

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.problem.Problem
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax._
import js7.data.agent.AgentPath
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.Event
import js7.data.lock.LockPath
import js7.data.order.Order._
import js7.data.orderwatch.ExternalOrderKey
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.WorkflowId
import js7.data.workflow.instructions.Fork
import js7.data.workflow.position.{Position, WorkflowPosition}

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event {
  type Key = OrderId
}

object OrderEvent
{
  sealed trait OrderCoreEvent extends OrderEvent
  sealed trait OrderActorEvent extends OrderCoreEvent
  sealed trait OrderTerminated extends OrderEvent

  final case class OrderAdded(
    workflowId: WorkflowId,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    externalOrderKey: Option[ExternalOrderKey] = None)
  extends OrderCoreEvent {
    workflowId.requireNonAnonymous()
  }
  object OrderAdded {
    private[OrderEvent] implicit val jsonCodec: Encoder.AsObject[OrderAdded] =
      o => JsonObject(
        "workflowId" -> o.workflowId.asJson,
        "scheduledFor" -> o.scheduledFor.asJson,
        "externalOrderKey" -> o.externalOrderKey.asJson,
        "arguments" -> o.arguments.??.asJson
      )

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderAdded] =
      c => for {
        workflowId <- c.get[WorkflowId]("workflowId")
        scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
        externalOrderKey <- c.get[Option[ExternalOrderKey]]("externalOrderKey")
        arguments <- c.getOrElse[NamedValues]("arguments")(Map.empty)
      } yield OrderAdded(workflowId, arguments, scheduledFor, externalOrderKey)
  }

  /** Agent-only event. */
  final case class OrderAttachedToAgent(
    workflowPosition: WorkflowPosition,
    state: IsFreshOrReady,
    arguments: NamedValues,
    scheduledFor: Option[Timestamp],
    externalOrderKey: Option[ExternalOrderKey],
    historicOutcomes: Seq[HistoricOutcome],
    agentPath: AgentPath,
    parent: Option[OrderId],
    mark: Option[OrderMark],
    isSuspended: Boolean,
    deleteWhenTerminated: Boolean)
  extends OrderCoreEvent {
    workflowPosition.workflowId.requireNonAnonymous()
  }

  final case class OrderAttached(agentPath: AgentPath)
  extends OrderCoreEvent

  type OrderStarted = OrderStarted.type
  case object OrderStarted extends OrderActorEvent

  type OrderProcessingStarted = OrderProcessingStarted.type
  case object OrderProcessingStarted extends OrderCoreEvent

  sealed trait OrderStdWritten extends OrderEvent {

    def stdoutStderr: StdoutOrStderr
    def chunk: String

    override def toString = getClass.simpleScalaName + "(" +
      chunk.truncateWithEllipsis(160, showLength = true).replace("\n", "\\n").replace("\r", "\\r") + ")"
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
    def stdoutStderr = Stdout
    override def toString = super.toString
  }

  final case class OrderStderrWritten(chunk: String) extends OrderStdWritten {
    def stdoutStderr = Stderr
    override def toString = super.toString
  }

  final case class OrderProcessed(outcome: Outcome) extends OrderCoreEvent

  final case class OrderForked(children: Seq[OrderForked.Child]) extends OrderActorEvent
  object OrderForked {
    final case class Child(branchId: Fork.Branch.Id, orderId: OrderId)
    object Child {
      implicit val jsonCodec = deriveCodec[Child]
    }
  }

  final case class OrderJoined(outcome: Outcome)
  extends OrderActorEvent

  final case class OrderOffered(orderId: OrderId, until: Timestamp)
  extends OrderActorEvent

  final case class OrderAwaiting(orderId: OrderId) extends OrderActorEvent

  type OrderProcessingKilled = OrderProcessingKilled.type
  final case object OrderProcessingKilled
  extends OrderActorEvent

  final case class OrderMoved(to: Position)
  extends OrderActorEvent

  sealed trait OrderLockEvent extends OrderActorEvent {
    def lockPaths: Seq[LockPath]
  }
  object OrderLockEvent {
    def unapply(event: OrderLockEvent) = Some(event.lockPaths)
  }

  sealed trait OrderFailedEvent extends OrderActorEvent with OrderLockEvent {
    def moveTo(movedTo: Position): OrderFailedEvent

    def movedTo: Position
  }

  final case class OrderFailed(movedTo: Position, outcome: Option[Outcome.NotSucceeded] = None, lockPaths: Seq[LockPath] = Nil)
  extends OrderFailedEvent with OrderTerminated {
    def moveTo(movedTo: Position) = copy(movedTo = movedTo)
  }

  final case class OrderFailedInFork(movedTo: Position, outcome: Option[Outcome.NotSucceeded] = None, lockPaths: Seq[LockPath] = Nil)
  extends OrderFailedEvent {
    def moveTo(movedTo: Position) = copy(movedTo = movedTo)
  }

  final case class OrderCatched(movedTo: Position, outcome: Option[Outcome.NotSucceeded] = None, lockPaths: Seq[LockPath] = Nil)
  extends OrderFailedEvent {
    def moveTo(movedTo: Position) = copy(movedTo = movedTo)
  }

  /** Only intermediate, not persisted. Will be converted to `OrderFailed` or `OrderCatched`. */
  final case class OrderFailedIntermediate_(outcome: Option[Outcome.NotSucceeded] = None, uncatchable: Boolean = false)
  extends OrderActorEvent

  final case class OrderRetrying(movedTo: Position, delayedUntil: Option[Timestamp] = None)
  extends OrderActorEvent

  type OrderAwoke = OrderAwoke.type
  case object OrderAwoke extends OrderActorEvent

  final case class OrderBroken(problem: Problem) extends OrderActorEvent

  /**
    * Controller may have started to attach Order to Agent..
    */
  final case class OrderAttachable(agentPath: AgentPath) extends OrderActorEvent

  type OrderDetachable = OrderDetachable.type
  /**
    * Agent has processed all steps and the Order should be fetched by the Controller.
    */
  case object OrderDetachable extends OrderActorEvent

  type OrderDetached = OrderDetached.type
  /**
    * Order has been removed from the Agent and is held by the Controller.
    * Agent-only event.
    */
  case object OrderDetached extends OrderCoreEvent

  type OrderFinished = OrderFinished.type
  case object OrderFinished extends OrderActorEvent with OrderTerminated

  type OrderDeletionMarked = OrderDeletionMarked.type
  case object OrderDeletionMarked extends OrderActorEvent

  type OrderDeleted = OrderDeleted.type
  case object OrderDeleted extends OrderActorEvent

  sealed trait OrderKillingMarked extends OrderActorEvent {
    def kill: Option[CancellationMode.Kill]
  }
  object OrderKillingMarked {
    def unapply(event: OrderKillingMarked) = Some(event.kill)
  }

  /** A OrderCancellationMarked on Agent is different from same Event at the Controller.
    * Controller will ignore the Agent's OrderCancellationMarked.
    * Controller should have emitted the event independendly.
    **/
  final case class OrderCancellationMarked(mode: CancellationMode) extends OrderKillingMarked {
    def kill = mode match {
      case CancellationMode.FreshOrStarted(kill) => kill
      case _ => None
    }
  }

  type OrderCancellationMarkedOnAgent = OrderCancellationMarkedOnAgent.type
  /** No other use than notifying an external user. */
  case object OrderCancellationMarkedOnAgent extends OrderActorEvent

  type OrderCancelled = OrderCancelled.type
  case object OrderCancelled extends OrderActorEvent with OrderTerminated

  final case class OrderSuspensionMarked(mode: SuspensionMode = SuspensionMode.standard)
  extends OrderKillingMarked {
    def kill = mode.kill
  }

  type OrderSuspensionMarkedOnAgent = OrderSuspensionMarkedOnAgent.type
  /** No other use than notifying an external user. */
  case object OrderSuspensionMarkedOnAgent extends OrderActorEvent

  type OrderSuspended = OrderSuspended.type
  case object OrderSuspended extends OrderActorEvent

  final case class OrderResumptionMarked(
    position: Option[Position] = None,
    historicOutcomes: Option[Seq[HistoricOutcome]] = None)
  extends OrderActorEvent

  final case class OrderResumed(
    position: Option[Position] = None,
    historicOutcomes: Option[Seq[HistoricOutcome]] = None)
  extends OrderActorEvent

  final case class OrderLockAcquired(lockPath: LockPath, count: Option[Int] = None)
  extends OrderLockEvent {
    def lockPaths = lockPath :: Nil
  }

  final case class OrderLockQueued(lockPath: LockPath, count: Option[Int])
  extends OrderLockEvent  {
    def lockPaths = lockPath :: Nil
  }

  final case class OrderLockReleased(lockPath: LockPath)
  extends OrderLockEvent {
    def lockPaths = lockPath :: Nil
  }

  final case class OrderPrompted(question: Value)
  extends OrderActorEvent

  final case class OrderPromptAnswered(/*outcome: Outcome.Completed*/)
  extends OrderCoreEvent

  implicit val jsonCodec = TypedJsonCodec[OrderEvent](
    Subtype[OrderAdded],
    Subtype(OrderDeletionMarked),
    Subtype(OrderDeleted),
    Subtype(OrderStarted),
    Subtype(OrderProcessingStarted),
    Subtype(deriveCodec[OrderStdoutWritten]),
    Subtype(deriveCodec[OrderStderrWritten]),
    Subtype(deriveCodec[OrderProcessed]),
    Subtype(deriveCodec[OrderCatched]),
    Subtype(deriveCodec[OrderRetrying]),
    Subtype(OrderAwoke),
    Subtype(deriveCodec[OrderProcessingKilled]),
    Subtype(deriveCodec[OrderMoved]),
    Subtype(deriveCodec[OrderForked]),
    Subtype(deriveCodec[OrderJoined]),
    Subtype(deriveCodec[OrderOffered]),
    Subtype(deriveCodec[OrderAwaiting]),
    Subtype(deriveCodec[OrderSuspensionMarked]),
    Subtype(OrderSuspensionMarkedOnAgent),
    Subtype(OrderSuspended),
    Subtype(deriveCodec[OrderResumptionMarked]),
    Subtype(deriveCodec[OrderResumed]),
    Subtype(OrderFinished),
    Subtype(deriveCodec[OrderFailed]),
    Subtype(deriveCodec[OrderFailedInFork]),
    Subtype(deriveCodec[OrderCancellationMarked]),
    Subtype(OrderCancellationMarkedOnAgent),
    Subtype(OrderCancelled),
    Subtype(deriveCodec[OrderAttached]),
    Subtype(deriveCodec[OrderAttachable]),
    Subtype(deriveCodec[OrderAttachedToAgent]),
    Subtype(OrderDetachable),
    Subtype(OrderDetached),
    Subtype(deriveCodec[OrderBroken]),
    Subtype(deriveCodec[OrderLockAcquired]),
    Subtype(deriveCodec[OrderLockQueued]),
    Subtype(deriveCodec[OrderLockReleased]),
    Subtype(deriveCodec[OrderPrompted]),
    Subtype(deriveCodec[OrderPromptAnswered]))
}
