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
import js7.data.command.{CancelMode, SuspendMode}
import js7.data.event.Event
import js7.data.lock.LockPath
import js7.data.order.Order._
import js7.data.orderwatch.ExternalOrderKey
import js7.data.value.NamedValues
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
        "arguments" -> o.arguments.??.asJson,
        "externalOrderKey" -> o.externalOrderKey.asJson)

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderAdded] =
      c => for {
        workflowId <- c.get[WorkflowId]("workflowId")
        scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
        arguments <- c.getOrElse[NamedValues]("arguments")(Map.empty)
        externalOrderKey <- c.get[Option[ExternalOrderKey]]("externalOrderKey")
      } yield OrderAdded(workflowId, arguments, scheduledFor, externalOrderKey)
  }

  /** Agent-only event. */
  final case class OrderAttachedToAgent(
    workflowPosition: WorkflowPosition,
    state: IsFreshOrReady,
    arguments: NamedValues,
    externalOrderKey: Option[ExternalOrderKey],
    historicOutcomes: Seq[HistoricOutcome],
    agentPath: AgentPath,
    parent: Option[OrderId],
    mark: Option[OrderMark],
    isSuspended: Boolean,
    removeWhenTerminated: Boolean)
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

  type OrderRemoveMarked = OrderRemoveMarked.type
  case object OrderRemoveMarked extends OrderActorEvent

  type OrderRemoved = OrderRemoved.type
  case object OrderRemoved extends OrderActorEvent

  sealed trait OrderKillMarked extends OrderActorEvent {
    def kill: Option[CancelMode.Kill]
  }
  object OrderKillMarked {
    def unapply(event: OrderKillMarked) = Some(event.kill)
  }

  /** A OrderCancelMarked on Agent is different from same Event at the Controller.
    * Controller will ignore the Agent's OrderCancelMarked.
    * Controller should have emitted the event independendly.
    **/
  final case class OrderCancelMarked(mode: CancelMode) extends OrderKillMarked {
    def kill = mode match {
      case CancelMode.FreshOrStarted(kill) => kill
      case _ => None
    }
  }

  type OrderCancelMarkedOnAgent = OrderCancelMarkedOnAgent.type
  /** No other use than notifying an external user. */
  case object OrderCancelMarkedOnAgent extends OrderActorEvent

  type OrderCancelled = OrderCancelled.type
  case object OrderCancelled extends OrderActorEvent with OrderTerminated

  final case class OrderSuspendMarked(mode: SuspendMode = SuspendMode.standard)
  extends OrderKillMarked {
    def kill = mode.kill
  }

  type OrderSuspendMarkedOnAgent = OrderSuspendMarkedOnAgent.type
  /** No other use than notifying an external user. */
  case object OrderSuspendMarkedOnAgent extends OrderActorEvent

  type OrderSuspended = OrderSuspended.type
  case object OrderSuspended extends OrderActorEvent

  final case class OrderResumeMarked(
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

  implicit val jsonCodec = TypedJsonCodec[OrderEvent](
    Subtype[OrderAdded],
    Subtype(OrderRemoveMarked),
    Subtype(OrderRemoved),
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
    Subtype(deriveCodec[OrderSuspendMarked]),
    Subtype(OrderSuspendMarkedOnAgent),
    Subtype(OrderSuspended),
    Subtype(deriveCodec[OrderResumeMarked]),
    Subtype(deriveCodec[OrderResumed]),
    Subtype(OrderFinished),
    Subtype(deriveCodec[OrderFailed]),
    Subtype(deriveCodec[OrderFailedInFork]),
    Subtype(deriveCodec[OrderCancelMarked]),
    Subtype(OrderCancelMarkedOnAgent),
    Subtype(OrderCancelled),
    Subtype(deriveCodec[OrderAttached]),
    Subtype(deriveCodec[OrderAttachable]),
    Subtype(deriveCodec[OrderAttachedToAgent]),
    Subtype(OrderDetachable),
    Subtype(OrderDetached),
    Subtype(deriveCodec[OrderBroken]),
    Subtype(deriveCodec[OrderLockAcquired]),
    Subtype(deriveCodec[OrderLockQueued]),
    Subtype(deriveCodec[OrderLockReleased]))
}
