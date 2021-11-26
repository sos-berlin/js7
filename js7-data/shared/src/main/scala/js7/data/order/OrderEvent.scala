package js7.data.order

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.problem.Problem
import js7.base.time.Timestamp
import js7.base.utils.Big
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax._
import js7.data.agent.AgentPath
import js7.data.board.{Notice, NoticeId}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.Event
import js7.data.lock.LockPath
import js7.data.order.Order._
import js7.data.orderwatch.ExternalOrderKey
import js7.data.subagent.SubagentId
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

  sealed trait OrderAddedX extends OrderCoreEvent {
    def workflowId: WorkflowId
    def arguments: NamedValues
    def scheduledFor: Option[Timestamp]
    def externalOrderKey: Option[ExternalOrderKey]
    def deleteWhenTerminated: Boolean
    def addedOrderId(orderId: OrderId): OrderId
  }

  final case class OrderAdded(
    workflowId: WorkflowId,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    externalOrderKey: Option[ExternalOrderKey] = None,
    deleteWhenTerminated: Boolean = false)
  extends OrderAddedX {
    workflowId.requireNonAnonymous()
    def addedOrderId(orderId: OrderId) = orderId
    override def toShortString = s"OrderAdded(${workflowId.path})"
  }
  object OrderAdded {
    private[OrderEvent] implicit val jsonCodec: Encoder.AsObject[OrderAdded] =
      o => JsonObject(
        "workflowId" -> o.workflowId.asJson,
        "scheduledFor" -> o.scheduledFor.asJson,
        "externalOrderKey" -> o.externalOrderKey.asJson,
        "arguments" -> o.arguments.??.asJson,
        "deleteWhenTerminated" -> o.deleteWhenTerminated.?.asJson
      )

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderAdded] =
      c => for {
        workflowId <- c.get[WorkflowId]("workflowId")
        scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
        externalOrderKey <- c.get[Option[ExternalOrderKey]]("externalOrderKey")
        arguments <- c.getOrElse[NamedValues]("arguments")(Map.empty)
        deleteWhenTerminated <- c.getOrElse[Boolean]("deleteWhenTerminated")(false)
      } yield OrderAdded(workflowId, arguments, scheduledFor, externalOrderKey, deleteWhenTerminated)
  }

  final case class OrderOrderAdded(
    orderId: OrderId,
    workflowId: WorkflowId,
    arguments: NamedValues = Map.empty,
    deleteWhenTerminated: Boolean = false)
  extends OrderAddedX with OrderActorEvent {
    workflowId.requireNonAnonymous()
    def scheduledFor = None
    def externalOrderKey = None
    def addedOrderId(o: OrderId) = orderId
    override def toShortString = s"OrderOrderAdded($orderId, ${workflowId.path})"
  }
  object OrderOrderAdded {
    private[OrderEvent] implicit val jsonCodec: Encoder.AsObject[OrderOrderAdded] =
      o => JsonObject(
        "orderId" -> o.orderId.asJson,
        "workflowId" -> o.workflowId.asJson,
        "arguments" -> o.arguments.??.asJson,
        "deleteWhenTerminated" -> o.deleteWhenTerminated.?.asJson)

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderOrderAdded] =
      c => for {
        orderId <- c.get[OrderId]("orderId")
        workflowId <- c.get[WorkflowId]("workflowId")
        arguments <- c.getOrElse[NamedValues]("arguments")(Map.empty)
        deleteWhenTerminated <- c.getOrElse[Boolean]("deleteWhenTerminated")(false)
      } yield OrderOrderAdded(orderId, workflowId, arguments, deleteWhenTerminated)
  }

  /** Agent-only event. */
  final case class OrderAttachedToAgent(
    workflowPosition: WorkflowPosition,
    state: IsFreshOrReady,
    arguments: NamedValues,
    scheduledFor: Option[Timestamp],
    externalOrderKey: Option[ExternalOrderKey],
    historicOutcomes: Vector[HistoricOutcome],
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

  // COMPATIBLE with v2.1
  val LegacySubagentId = SubagentId("JS7-2.1-LEGACY")

  final case class OrderProcessingStarted(subagentId: SubagentId)
  extends OrderCoreEvent
  object OrderProcessingStarted {
    implicit val jsonEncoder: Encoder.AsObject[OrderProcessingStarted] =
      o => JsonObject.fromIterable(
        (o.subagentId != LegacySubagentId) ? ("subagentId" -> o.subagentId.asJson))

    implicit val jsonDecoder: Decoder[OrderProcessingStarted] =
      c => for {
        subagentId <- c.getOrElse("subagentId")(LegacySubagentId)
      } yield OrderProcessingStarted(subagentId)
  }

  sealed trait OrderStdWritten extends OrderEvent {

    def stdoutStderr: StdoutOrStderr
    def chunk: String

    override def toString = getClass.simpleScalaName + "(" +
      chunk
        .truncateWithEllipsis(
          300/*for error lines with long (file path) prefix*/,
          showLength = true,
          firstLineOnly = true)
        .replace("\n", "\\n")
        .replace("\r", "\\r") + ")"
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
  object OrderStdoutWritten {
    implicit val jsonCodec = deriveCodec[OrderStdoutWritten]
  }

  final case class OrderStderrWritten(chunk: String) extends OrderStdWritten {
    def stdoutStderr = Stderr
    override def toString = super.toString
  }
  object OrderStderrWritten {
    implicit val jsonCodec = deriveCodec[OrderStderrWritten]
  }

  final case class OrderProcessed(outcome: Outcome) extends OrderCoreEvent
  {
    override def isFailed = outcome.isFailed
  }
  object OrderProcessed {
    implicit val jsonCodec = deriveCodec[OrderProcessed]
  }

  final case class OrderForked(children: Vector[OrderForked.Child]) extends OrderActorEvent
  object OrderForked {
    final case class Child(
      orderId: OrderId,
      arguments: Map[String, Value],
      branchId: Option[Fork.Branch.Id] = None)

    object Child {
      def apply(branchId: Fork.Branch.Id, orderId: OrderId) =
        new Child(orderId, Map.empty, Some(branchId))

      implicit val jsonEncoder: Encoder.AsObject[Child] =
        o => JsonObject(
          "orderId" -> o.orderId.asJson,
          "arguments" -> o.arguments.??.asJson,
          "branchId" -> o.branchId.asJson)

      implicit val jsonDecoder: Decoder[Child] =
        c => for {
          orderId <- c.get[OrderId]("orderId")
          arguments <- c.getOrElse[Map[String, Value]]("arguments")(Map.empty)
          branchId <- c.get[Option[Fork.Branch.Id]]("branchId")
        } yield Child(orderId, arguments, branchId)
    }
  }

  final case class OrderJoined(outcome: Outcome)
  extends OrderActorEvent
  {
    override def isFailed = outcome.isFailed
  }

  sealed trait OrderNoticeEvent extends OrderActorEvent

  final case class OrderNoticePosted(notice: Notice)
  extends OrderNoticeEvent

  final case class OrderNoticeExpected(noticeId: NoticeId)
  extends OrderNoticeEvent

  type OrderNoticeRead = OrderNoticeRead.type
  case object OrderNoticeRead
  extends OrderNoticeEvent

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

  sealed trait OrderFailedEvent extends OrderActorEvent {
    def moveTo(movedTo: Position): OrderFailedEvent

    def movedTo: Position
  }

  final case class OrderFailed(
    movedTo: Position,
    outcome: Option[Outcome.NotSucceeded] = None)
  extends OrderFailedEvent with OrderTerminated {
    def moveTo(movedTo: Position) = copy(movedTo = movedTo)
  }

  final case class OrderFailedInFork(
    movedTo: Position,
    outcome: Option[Outcome.NotSucceeded] = None)
  extends OrderFailedEvent {
    def moveTo(movedTo: Position) = copy(movedTo = movedTo)
  }

  final case class OrderCatched(
    movedTo: Position,
    outcome: Option[Outcome.NotSucceeded] = None)
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

  final case class OrderBroken(problem: Problem) extends OrderActorEvent {
    override def toString = s"💥 OrderBroken($problem)"
  }

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
  final case class OrderCancellationMarked(mode: CancellationMode = CancellationMode.Default)
  extends OrderKillingMarked {
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
    historyOperations: Seq[OrderResumed.HistoryOperation] = Nil)
  extends OrderActorEvent with Big

  final case class OrderResumed(
    position: Option[Position] = None,
    historyOperations: Seq[OrderResumed.HistoryOperation] = Nil)
  extends OrderActorEvent with Big
  object OrderResumed
  {
    sealed trait HistoryOperation {
      def positions: Iterable[Position]
    }

    final case class ReplaceHistoricOutcome(position: Position, outcome: Outcome)
    extends HistoryOperation {
      def positions = position :: Nil
    }
    object ReplaceHistoricOutcome {
      def apply(historicOutcome: HistoricOutcome) =
        new ReplaceHistoricOutcome(historicOutcome.position, historicOutcome.outcome)
    }

    final case class InsertHistoricOutcome(before: Position, position: Position, outcome: Outcome)
    extends HistoryOperation {
      def positions = before :: position :: Nil
    }
    object InsertHistoricOutcome {
      def apply(at: Position, historicOutcome: HistoricOutcome) =
        new InsertHistoricOutcome(at, historicOutcome.position, historicOutcome.outcome)
    }

    final case class DeleteHistoricOutcome(position: Position)
    extends HistoryOperation {
      def positions = position :: Nil
    }

    final case class AppendHistoricOutcome(position: Position, outcome: Outcome)
    extends HistoryOperation {
      def positions = position :: Nil
    }
    object AppendHistoricOutcome {
      def apply(historicOutcome: HistoricOutcome) =
        new AppendHistoricOutcome(historicOutcome.position, historicOutcome.outcome)
    }

    object HistoryOperation {
      implicit val jsonCodec = TypedJsonCodec[HistoryOperation](
        Subtype.named(deriveCodec[ReplaceHistoricOutcome], "Replace"),
        Subtype.named(deriveCodec[InsertHistoricOutcome], "Insert"),
        Subtype.named(deriveCodec[DeleteHistoricOutcome], "Delete"),
        Subtype.named(deriveCodec[AppendHistoricOutcome], "Append"))
    }
  }

  final case class OrderLockAcquired(lockPath: LockPath, count: Option[Int] = None)
  extends OrderLockEvent {
    def lockPaths = lockPath :: Nil
  }

  final case class OrderLockQueued(lockPath: LockPath, count: Option[Int])
  extends OrderLockEvent  {
    def lockPaths = lockPath :: Nil
  }

  final case class OrderLockDequeued(lockPath: LockPath)
  extends OrderLockEvent {
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

  final case class OrderCyclingPrepared(cycleState: CycleState)
  extends OrderActorEvent

  type OrderCycleStarted = OrderCycleStarted.type
  final case object OrderCycleStarted
  extends OrderActorEvent

  final case class OrderCycleFinished(cycleState: Option[CycleState])
  extends OrderActorEvent

  implicit val jsonCodec = TypedJsonCodec[OrderEvent](
    Subtype[OrderAdded],
    Subtype[OrderOrderAdded],
    Subtype(OrderDeletionMarked),
    Subtype(OrderDeleted),
    Subtype(OrderStarted),
    Subtype[OrderProcessingStarted],
    Subtype[OrderStdoutWritten],
    Subtype[OrderStderrWritten],
    Subtype[OrderProcessed],
    Subtype(deriveCodec[OrderCatched]),
    Subtype(deriveCodec[OrderRetrying]),
    Subtype(OrderAwoke),
    Subtype(deriveCodec[OrderProcessingKilled]),
    Subtype(deriveCodec[OrderMoved]),
    Subtype(deriveCodec[OrderForked]),
    Subtype(deriveCodec[OrderJoined]),
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
    Subtype(deriveCodec[OrderLockDequeued]),
    Subtype(deriveCodec[OrderLockReleased]),
    Subtype(deriveCodec[OrderNoticePosted]),
    Subtype(deriveCodec[OrderNoticeExpected]),
    Subtype(OrderNoticeRead),
    Subtype(deriveCodec[OrderPrompted]),
    Subtype(deriveCodec[OrderPromptAnswered]),
    Subtype(deriveCodec[OrderCyclingPrepared]),
    Subtype(OrderCycleStarted),
    Subtype(deriveCodec[OrderCycleFinished]))
}
