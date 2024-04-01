package js7.data.order

import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.{RichCirceObjectCodec, deriveConfiguredCodec}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Big
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, Notice, NoticeId, NoticeV2_3}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.Event
import js7.data.lock.LockPath
import js7.data.order.Order.*
import js7.data.order.OrderEvent.OrderMoved.Reason
import js7.data.orderwatch.ExternalOrderKey
import js7.data.subagent.Problems.{ProcessLostDueToResetProblem, ProcessLostDueToRestartProblem}
import js7.data.subagent.{SubagentId, SubagentSelectionId}
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.WorkflowId
import js7.data.workflow.instructions.Fork
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchPath, Position, PositionOrLabel, WorkflowPosition}
import org.jetbrains.annotations.TestOnly
import scala.annotation.nowarn
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event.IsKeyBase[OrderEvent]:
  val keyCompanion: OrderEvent.type = OrderEvent


object OrderEvent extends Event.CompanionForKey[OrderId, OrderEvent]:
  implicit def implicitSelf: OrderEvent.type = this

  sealed trait OrderCoreEvent extends OrderEvent
  sealed trait OrderActorEvent extends OrderCoreEvent
  sealed trait OrderTerminated extends OrderEvent

  sealed trait OrderAddedX extends OrderCoreEvent:
    def workflowId: WorkflowId
    def arguments: NamedValues
    def scheduledFor: Option[Timestamp]
    def externalOrderKey: Option[ExternalOrderKey]
    def deleteWhenTerminated: Boolean
    def forceJobAdmission: Boolean
    def innerBlock: BranchPath
    def startPosition: Option[Position]
    def stopPositions: Set[PositionOrLabel]
    def addedOrderId(orderId: OrderId): OrderId

  final case class OrderAdded(
    workflowId: WorkflowId,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    externalOrderKey: Option[ExternalOrderKey] = None,
    deleteWhenTerminated: Boolean = false,
    forceJobAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[Position] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  extends OrderAddedX:
    workflowId.requireNonAnonymous()
    def addedOrderId(orderId: OrderId) = orderId
    override def toShortString = s"OrderAdded(${workflowId.path})"
  object OrderAdded:

    private[OrderEvent] implicit val jsonCodec: Encoder.AsObject[OrderAdded] =
      o => JsonObject(
        "workflowId" -> o.workflowId.asJson,
        "scheduledFor" -> o.scheduledFor.asJson,
        "externalOrderKey" -> o.externalOrderKey.asJson,
        "arguments" -> o.arguments.??.asJson,
        "deleteWhenTerminated" -> o.deleteWhenTerminated.?.asJson,
        "forceJobAdmission" -> o.forceJobAdmission.?.asJson,
        "innerBlock" -> (o.innerBlock.nonEmpty ? o.innerBlock).asJson,
        "startPosition" -> o.startPosition.asJson,
        "stopPositions" -> (o.stopPositions.nonEmpty ? o.stopPositions).asJson)

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderAdded] =
      c => for
        workflowId <- c.get[WorkflowId]("workflowId")
        scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
        externalOrderKey <- c.get[Option[ExternalOrderKey]]("externalOrderKey")
        arguments <- c.getOrElse[NamedValues]("arguments")(Map.empty)
        deleteWhenTerminated <- c.getOrElse[Boolean]("deleteWhenTerminated")(false)
        forceJobAdmission <- c.getOrElse[Boolean]("forceJobAdmission")(false)
        innerBlock <- c.getOrElse[BranchPath]("innerBlock")(BranchPath.empty)
        startPosition <- c.get[Option[Position]]("startPosition")
        stopPositions <- c.getOrElse[Set[PositionOrLabel]]("stopPositions")(Set.empty)
      yield OrderAdded(workflowId, arguments, scheduledFor, externalOrderKey,
        deleteWhenTerminated, forceJobAdmission,
        innerBlock, startPosition, stopPositions)

  /** Event for the AddOrder instruction. */
  final case class OrderOrderAdded(
    orderId: OrderId,
    workflowId: WorkflowId,
    arguments: NamedValues = Map.empty,
    deleteWhenTerminated: Boolean = false,
    forceJobAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[Position] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  extends OrderAddedX, OrderActorEvent:
    workflowId.requireNonAnonymous()
    def scheduledFor = None
    def externalOrderKey = None
    def addedOrderId(o: OrderId) = orderId
    override def toShortString = s"OrderOrderAdded($orderId, ${workflowId.path})"
  object OrderOrderAdded:

    private[OrderEvent] implicit val jsonCodec: Encoder.AsObject[OrderOrderAdded] =
      o => JsonObject(
        "orderId" -> o.orderId.asJson,
        "workflowId" -> o.workflowId.asJson,
        "arguments" -> o.arguments.??.asJson,
        "deleteWhenTerminated" -> o.deleteWhenTerminated.?.asJson,
        "forceJobAdmission" -> o.forceJobAdmission.?.asJson,
        "innerBlock" -> (o.innerBlock.nonEmpty ? o.innerBlock).asJson,
        "startPosition" -> o.startPosition.asJson,
        "stopPositions" -> o.stopPositions.??.asJson)

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderOrderAdded] =
      c => for
        orderId <- c.get[OrderId]("orderId")
        workflowId <- c.get[WorkflowId]("workflowId")
        arguments <- c.getOrElse[NamedValues]("arguments")(Map.empty)
        innerBlock <- c.getOrElse[BranchPath]("innerBlock")(BranchPath.empty)
        startPosition <- c.get[Option[Position]]("startPosition")
        stopPositions <- c.getOrElse[Set[PositionOrLabel]]("stopPositions")(Set.empty)
        deleteWhenTerminated <- c.getOrElse[Boolean]("deleteWhenTerminated")(false)
        forceJobAdmission <- c.getOrElse[Boolean]("forceJobAdmission")(false)
      yield OrderOrderAdded(orderId, workflowId, arguments,
        deleteWhenTerminated, forceJobAdmission,
        innerBlock, startPosition, stopPositions)

  /** Agent-only event. */
  final case class OrderAttachedToAgent(
    workflowPosition: WorkflowPosition,
    state: IsFreshOrReady,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    externalOrderKey: Option[ExternalOrderKey] = None,
    historicOutcomes: Vector[HistoricOutcome] = Vector.empty,
    agentPath: AgentPath,
    parent: Option[OrderId] = None,
    mark: Option[OrderMark] = None,
    isSuspended: Boolean = false,
    isResumed: Boolean = false,
    deleteWhenTerminated: Boolean = false,
    forceJobAdmission: Boolean = false,
    stickySubagents: List[Order.StickySubagent] = Nil,
    innerBlock: BranchPath = BranchPath.empty,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  extends OrderCoreEvent:
    workflowPosition.workflowId.requireNonAnonymous()

    def workflowId = workflowPosition.workflowId

  final case class OrderAttached(agentPath: AgentPath)
  extends OrderCoreEvent

  type OrderStarted = OrderStarted.type
  case object OrderStarted extends OrderActorEvent

  // subagentId = None is COMPATIBLE with v2.2
  final case class OrderProcessingStarted(subagentId: Option[SubagentId], stick: Boolean = false)
  extends OrderCoreEvent:
    override def toString =
      s"OrderProcessingStarted(${subagentId getOrElse "legacy local Subagent"}${stick ?? " stick"})"
  object OrderProcessingStarted:
    // Since v2.3
    @TestOnly
    def apply(subagentId: SubagentId): OrderProcessingStarted =
      OrderProcessingStarted(Some(subagentId))

    // Since v2.3
    def apply(subagentId: SubagentId, stick: Boolean): OrderProcessingStarted =
      OrderProcessingStarted(Some(subagentId), stick)

  sealed trait OrderStdWritten extends OrderEvent:
    def stdoutStderr: StdoutOrStderr
    def chunk: String

    override def toString = getClass.simpleScalaName + "(" +
      chunk.truncateWithEllipsis(
        300/*for error lines with long (file path) prefix*/,
        showLength = true) + ")"
  object OrderStdWritten:
    def apply(t: StdoutOrStderr): String => OrderStdWritten =
      t match
        case Stdout => OrderStdoutWritten.apply
        case Stderr => OrderStderrWritten.apply

    def unapply(o: OrderStdWritten) = o match
      case OrderStdoutWritten(chunk) => Some((Stdout, chunk))
      case OrderStderrWritten(chunk) => Some((Stderr, chunk))

  final case class OrderStdoutWritten(chunk: String) extends OrderStdWritten:
    def stdoutStderr = Stdout
    override def toString = super.toString
  object OrderStdoutWritten:
    implicit val jsonCodec: Codec.AsObject[OrderStdoutWritten] = deriveCodec[OrderStdoutWritten]

  final case class OrderStderrWritten(chunk: String) extends OrderStdWritten:
    def stdoutStderr = Stderr
    override def toString = super.toString
  object OrderStderrWritten:
    implicit val jsonCodec: Codec.AsObject[OrderStderrWritten] = deriveCodec[OrderStderrWritten]

  final case class OrderProcessed(outcome: Outcome) extends OrderCoreEvent:
    override def isSucceeded = outcome.isSucceeded
  object OrderProcessed:
    val processLostDueToRestart = processLost(ProcessLostDueToRestartProblem)
    val processLostDueToReset = processLost(ProcessLostDueToResetProblem)

    def processLost(problem: Problem) =
      OrderProcessed(Outcome.Disrupted(Outcome.Disrupted.ProcessLost(problem)))

    implicit val jsonCodec: Codec.AsObject[OrderProcessed] = deriveCodec[OrderProcessed]

  final case class OrderForked(children: Vector[OrderForked.Child]) extends OrderActorEvent
  object OrderForked:
    final case class Child(
      orderId: OrderId,
      arguments: Map[String, Value],
      branchId: Option[Fork.Branch.Id] = None)

    object Child:
      def apply(branchId: Fork.Branch.Id, orderId: OrderId): Child =
        new Child(orderId, Map.empty, Some(branchId))

      implicit def fromPair(pair: (String, OrderId)): Child =
        apply(Fork.Branch.Id(pair._1), pair._2)

      implicit val jsonEncoder: Encoder.AsObject[Child] =
        o => JsonObject(
          "orderId" -> o.orderId.asJson,
          "arguments" -> o.arguments.??.asJson,
          "branchId" -> o.branchId.asJson)

      implicit val jsonDecoder: Decoder[Child] =
        c => for
          orderId <- c.get[OrderId]("orderId")
          arguments <- c.getOrElse[Map[String, Value]]("arguments")(Map.empty)
          branchId <- c.get[Option[Fork.Branch.Id]]("branchId")
        yield Child(orderId, arguments, branchId)

  final case class OrderJoined(outcome: Outcome)
  extends OrderActorEvent:
    override def isSucceeded = outcome.isSucceeded

  sealed trait OrderNoticeEvent extends OrderActorEvent

  sealed trait OrderNoticePosted_ extends OrderNoticeEvent
  object OrderNoticePosted_ :
    private val jsonEncoder: Encoder.AsObject[OrderNoticePosted_] =
      case o: OrderNoticePostedV2_3 => OrderNoticePostedV2_3.jsonEncoder.encodeObject(o)
      case o: OrderNoticePosted => OrderNoticePosted.jsonEncoder.encodeObject(o)

    private val jsonDecoder: Decoder[OrderNoticePosted_] = c =>
      if c.value.asObject.flatMap(_("notice")).flatMap(_.asObject).exists(_.contains("boardPath")) then
        c.get[Notice]("notice").map(OrderNoticePosted(_))
      else
        c.get[NoticeV2_3]("notice").map(OrderNoticePostedV2_3(_))

    implicit val jsonCodec: Codec.AsObject[OrderNoticePosted_] =
      Codec.AsObject.from(jsonDecoder, jsonEncoder)

  // COMPATIBLE with v2.3
  final case class OrderNoticePostedV2_3(notice: NoticeV2_3)
  extends OrderNoticePosted_
  object OrderNoticePostedV2_3:
    implicit val jsonEncoder: Encoder.AsObject[OrderNoticePostedV2_3] = deriveEncoder

  final case class OrderNoticePosted(notice: Notice)
  extends OrderNoticePosted_
  object OrderNoticePosted:
    implicit val jsonEncoder: Encoder.AsObject[OrderNoticePosted] = deriveEncoder

  // COMPATIBLE with v2.3
  final case class OrderNoticeExpected(noticeId: NoticeId)
  extends OrderNoticeEvent

  final case class OrderNoticesExpected(expected: Vector[OrderNoticesExpected.Expected])
  extends OrderNoticeEvent
  object OrderNoticesExpected:
    final case class Expected(boardPath: BoardPath, noticeId: NoticeId)
    object Expected:
      implicit val jsonCodec: Codec.AsObject[Expected] = deriveCodec

  type OrderNoticesRead = OrderNoticesRead.type
  case object OrderNoticesRead
  extends OrderNoticeEvent

  final case class OrderNoticesConsumptionStarted(
    consumptions: Vector[OrderNoticesConsumptionStarted.Consumption])
  extends OrderNoticeEvent:
    def checked: Checked[this.type] =
      consumptions.checkUniqueness(_.boardPath).>>(
        if consumptions.isEmpty then
          Problem.pure("Invalid arguments for OrderNoticesConsumptionStarted")
        else
          Right(this))
  object OrderNoticesConsumptionStarted:
    implicit val jsonCodec: Codec.AsObject[OrderNoticesConsumptionStarted] =
      deriveCodec[OrderNoticesConsumptionStarted].checked(_.checked)
    type Consumption = OrderNoticesExpected.Expected
    val Consumption = OrderNoticesExpected.Expected

  final case class OrderNoticesConsumed(failed: Boolean = false)
  extends OrderNoticeEvent

  // Similar to OrderProcessingCancelled, but only for the Processing state.
  // Maybe we want to differentiate between cancellation of an order and killing of a process.
  type OrderProcessingKilled = OrderProcessingKilled.type
  case object OrderProcessingKilled
  extends OrderActorEvent

  final case class OrderStickySubagentEntered(
    agentPath: AgentPath,
    subagentSelectionId: Option[SubagentSelectionId] = None)
  extends OrderActorEvent

  case object OrderStickySubagentLeaved
  extends OrderActorEvent

  final case class OrderMoved(to: Position, reason: Option[Reason] = None)
  extends OrderActorEvent
  object OrderMoved:
    sealed trait Reason
    case object SkippedDueToWorkflowPathControl extends Reason
    case object NoAdmissionPeriodStart extends Reason

    implicit val jsonCodec: TypedJsonCodec[Reason] = TypedJsonCodec(
      Subtype(SkippedDueToWorkflowPathControl),
      Subtype(NoAdmissionPeriodStart))

  sealed trait OrderFailedEvent extends OrderActorEvent:
    def moveTo(movedTo: Position): OrderFailedEvent

    def movedTo: Position

  final case class OrderOutcomeAdded(outcome: Outcome)
  extends OrderActorEvent

  final case class OrderFailed(
    movedTo: Position,
    // COMPATIBLE with v2.4: outcome has been replaced by OrderOutcomeAdded event
    outcome: Option[Outcome.NotSucceeded])
  extends OrderFailedEvent, OrderTerminated:
    def moveTo(movedTo: Position) = copy(movedTo = movedTo)
  object OrderFailed:
    @deprecated("outcome is deprecated", "v2.5")
    private[order] def apply(movedTo: Position, outcome: Option[Outcome.NotSucceeded]) =
      new OrderFailed(movedTo, outcome)

    def apply(movedTo: Position) = new OrderFailed(movedTo, None)

  final case class OrderFailedInFork(
    movedTo: Position,
    // outcome has been replaced by OrderOutcomeAdded event. COMPATIBLE with v2.4
    outcome: Option[Outcome.NotSucceeded] = None)
  extends OrderFailedEvent:
    def moveTo(movedTo: Position) = copy(movedTo = movedTo)
  object OrderFailedInFork:
    @deprecated("outcome is deprecated", "v2.5")
    private[order] def apply(movedTo: Position, outcome: Option[Outcome.NotSucceeded]) =
      new OrderFailedInFork(movedTo, outcome)

    def apply(movedTo: Position) = new OrderFailedInFork(movedTo, None)

  // COMPATIBLE with v2.4
  final case class OrderCatched(
    movedTo: Position,
    outcome: Option[Outcome.NotSucceeded] = None)
  extends OrderFailedEvent:
    def moveTo(movedTo: Position) = copy(movedTo = movedTo)

  final case class OrderCaught(
    movedTo: Position,
    // COMPATIBLE with v2.4: outcome has been replaced by OrderOutcomeAdded event
    outcome: Option[Outcome.NotSucceeded] = None)
  extends OrderFailedEvent:
    def moveTo(movedTo: Position) = copy(movedTo = movedTo)
  object OrderCaught:
    @deprecated("outcome is deprecated", "v2.5")
    private[order] def apply(movedTo: Position, outcome: Option[Outcome.NotSucceeded]) =
      new OrderCaught(movedTo, outcome)

    def apply(movedTo: Position) = new OrderCaught(movedTo, None)

  /** Only intermediate, not persisted. Will be converted to `OrderFailed`, `OrderCaught` or
   * `OrderStopped`,  */
  final case class OrderFailedIntermediate_(outcome: Option[Outcome.NotSucceeded] = None)
  extends OrderActorEvent

  final case class OrderRetrying(movedTo: Position, delayedUntil: Option[Timestamp] = None)
  extends OrderActorEvent

  type OrderAwoke = OrderAwoke.type
  case object OrderAwoke extends OrderActorEvent

  final case class OrderBroken(problem: Option[Problem]) extends OrderActorEvent:
    override def toString = s"💥 OrderBroken($problem)"
  object OrderBroken:
    // COMPATIBLE with v2.4
    @deprecated("outcome is deprecated", "v2.5")
    private[order] def apply(problem: Problem): OrderBroken =
      OrderBroken(Some(problem))

    def apply(): OrderBroken =
      OrderBroken(None)

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

  final case class OrderFinished(outcome: Option[Outcome.Completed] = None)
  extends OrderActorEvent, OrderTerminated

  type OrderDeletionMarked = OrderDeletionMarked.type
  case object OrderDeletionMarked extends OrderActorEvent

  type OrderDeleted = OrderDeleted.type
  case object OrderDeleted extends OrderActorEvent

  sealed trait OrderKillingMarked extends OrderActorEvent:
    def kill: Option[CancellationMode.Kill]
  object OrderKillingMarked:
    def unapply(event: OrderKillingMarked) = Some(event.kill)

  /** A OrderCancellationMarked on Agent is different from same Event at the Controller.
    * Controller will ignore the Agent's OrderCancellationMarked.
    * Controller should have emitted the event independendly.
    **/
  final case class OrderCancellationMarked(mode: CancellationMode = CancellationMode.Default)
  extends OrderKillingMarked:
    def kill = mode match
      case CancellationMode.FreshOrStarted(kill) => kill
      case _ => None

  type OrderCancellationMarkedOnAgent = OrderCancellationMarkedOnAgent.type
  /** No other use than notifying an external user. */
  case object OrderCancellationMarkedOnAgent extends OrderActorEvent

  type OrderOperationCancelled = OrderOperationCancelled.type
  case object OrderOperationCancelled extends OrderActorEvent

  type OrderCancelled = OrderCancelled.type
  case object OrderCancelled extends OrderActorEvent, OrderTerminated

  final case class OrderSuspensionMarked(mode: SuspensionMode = SuspensionMode.standard)
  extends OrderKillingMarked:
    def kill = mode.kill

  final case class OrderGoMarked(position: Position)
  extends OrderActorEvent

  type OrderGoes = OrderGoes.type
  case object OrderGoes
  extends OrderActorEvent

  type OrderSuspensionMarkedOnAgent = OrderSuspensionMarkedOnAgent.type
  /** No other use than notifying an external user. */
  case object OrderSuspensionMarkedOnAgent extends OrderActorEvent

  type OrderSuspended = OrderSuspended.type
  case object OrderSuspended extends OrderActorEvent

  type OrderStopped = OrderStopped.type
  case object OrderStopped extends OrderActorEvent

  final case class OrderResumptionMarked(
    position: Option[Position] = None,
    historyOperations: Seq[OrderResumed.HistoryOperation] = Nil,
    asSucceeded: Boolean = false)
  extends OrderActorEvent, Big

  final case class OrderResumed(
    position: Option[Position] = None,
    historyOperations: Seq[OrderResumed.HistoryOperation] = Nil,
    asSucceeded: Boolean = false)
  extends OrderActorEvent, Big
  object OrderResumed:
    sealed trait HistoryOperation:
      def positions: Iterable[Position]

    final case class ReplaceHistoricOutcome(position: Position, outcome: Outcome)
    extends HistoryOperation:
      def positions = position :: Nil
    object ReplaceHistoricOutcome:
      def apply(historicOutcome: HistoricOutcome) =
        new ReplaceHistoricOutcome(historicOutcome.position, historicOutcome.outcome)

    final case class InsertHistoricOutcome(before: Position, position: Position, outcome: Outcome)
    extends HistoryOperation:
      def positions = before :: position :: Nil
    object InsertHistoricOutcome:
      def apply(at: Position, historicOutcome: HistoricOutcome) =
        new InsertHistoricOutcome(at, historicOutcome.position, historicOutcome.outcome)

    final case class DeleteHistoricOutcome(position: Position)
    extends HistoryOperation:
      def positions = position :: Nil

    final case class AppendHistoricOutcome(position: Position, outcome: Outcome)
    extends HistoryOperation:
      def positions = position :: Nil
    object AppendHistoricOutcome:
      def apply(historicOutcome: HistoricOutcome) =
        new AppendHistoricOutcome(historicOutcome.position, historicOutcome.outcome)

    object HistoryOperation:
      implicit val jsonCodec: TypedJsonCodec[HistoryOperation] = TypedJsonCodec(
        Subtype.named(deriveCodec[ReplaceHistoricOutcome], "Replace"),
        Subtype.named(deriveCodec[InsertHistoricOutcome], "Insert"),
        Subtype.named(deriveCodec[DeleteHistoricOutcome], "Delete"),
        Subtype.named(deriveCodec[AppendHistoricOutcome], "Append"))

  sealed trait LegacyOrderLockEvent extends OrderActorEvent

  // COMPATIBLE with v2.4
  private final case class OrderLockAcquired(lockPath: LockPath, count: Option[Int])
  extends LegacyOrderLockEvent

  // COMPATIBLE with v2.4
  private final case class OrderLockQueued(lockPath: LockPath, count: Option[Int])
  extends LegacyOrderLockEvent

  // COMPATIBLE with v2.4
  private final case class OrderLockDequeued(lockPath: LockPath)
  extends LegacyOrderLockEvent

  // COMPATIBLE with v2.4
  private final case class OrderLockReleased(lockPath: LockPath)
  extends LegacyOrderLockEvent

  sealed trait OrderLockEvent extends OrderActorEvent:
    def lockPaths: Seq[LockPath]

  object OrderLockEvent:
    def unapply(event: OrderLockEvent) = Some(event.lockPaths)

  sealed trait OrderLockAcquiredOrQueuedEvent extends OrderLockEvent:
    def demands: Seq[LockDemand]

  final case class OrderLocksQueued(demands: List[LockDemand])
  extends OrderLockAcquiredOrQueuedEvent :
    def checked = LockDemand.checked(demands).rightAs(this)
    def lockPaths = demands.map(_.lockPath)

  final case class OrderLocksAcquired(demands: List[LockDemand])
    extends OrderLockAcquiredOrQueuedEvent:
    def checked = LockDemand.checked(demands).rightAs(this)
    def lockPaths = demands.map(_.lockPath)

  final case class OrderLocksDequeued(lockPaths: Seq[LockPath])
  extends OrderLockEvent:
    def checked = lockPaths.checkUniqueness.rightAs(this)

  final case class OrderLocksReleased(lockPaths: Seq[LockPath])
  extends OrderLockEvent:
    def checked = lockPaths.checkUniqueness.rightAs(this)

  final case class LockDemand(lockPath: LockPath, count: Option[Int] = None):
    def checked: Checked[this.type] =
      if count.exists(_ < 1) then
        Left(Problem(s"LockDemand.count must not be below 1 for $lockPath"))
      else
        Right(this)
  object LockDemand:
    def checked(demands: Seq[LockDemand]) =
      demands
        .checkUniqueness(_.lockPath)
        .>>(demands.traverse(_.checked))
        .rightAs(())

    implicit val jsonCodec: Codec.AsObject[LockDemand] = deriveCodec

  final case class OrderPrompted(question: Value)
  extends OrderActorEvent

  final case class OrderPromptAnswered(/*outcome: Outcome.Completed*/)
  extends OrderCoreEvent

  final case class OrderCyclingPrepared(cycleState: CycleState)
  extends OrderActorEvent

  type OrderCycleStarted = OrderCycleStarted.type
  case object OrderCycleStarted
  extends OrderActorEvent

  final case class OrderCycleFinished(cycleState: Option[CycleState])
  extends OrderActorEvent

  final case class OrderTransferred(workflowPosition: WorkflowPosition)
  extends OrderActorEvent

  @nowarn("msg=deprecated")
  implicit val jsonCodec: TypedJsonCodec[OrderEvent] = TypedJsonCodec(
    Subtype[OrderAdded],
    Subtype[OrderOrderAdded],
    Subtype(OrderDeletionMarked),
    Subtype(OrderDeleted),
    Subtype(OrderStarted),
    Subtype(deriveConfiguredCodec[OrderProcessingStarted]),
    Subtype[OrderStdoutWritten],
    Subtype[OrderStderrWritten],
    Subtype[OrderProcessed],
    Subtype(deriveCodec[OrderCatched]),
    Subtype(deriveCodec[OrderCaught]),
    Subtype(deriveCodec[OrderRetrying]),
    Subtype(OrderAwoke),
    Subtype(deriveCodec[OrderProcessingKilled]),
    Subtype(deriveCodec[OrderMoved]),
    Subtype(deriveCodec[OrderForked]),
    Subtype(deriveCodec[OrderJoined]),
    Subtype(deriveConfiguredCodec[OrderSuspensionMarked]),
    Subtype(OrderSuspensionMarkedOnAgent),
    Subtype(OrderSuspended),
    Subtype(OrderStopped),
    Subtype(deriveConfiguredCodec[OrderGoMarked]),
    Subtype(OrderGoes),
    Subtype(deriveConfiguredCodec[OrderResumptionMarked]),
    Subtype(deriveConfiguredCodec[OrderResumed]),
    Subtype(deriveConfiguredCodec[OrderFinished]),
    Subtype(deriveCodec[OrderOutcomeAdded]),
    Subtype(deriveCodec[OrderFailed]),
    Subtype(deriveCodec[OrderFailedInFork]),
    Subtype(deriveConfiguredCodec[OrderCancellationMarked]),
    Subtype(OrderOperationCancelled),
    Subtype(OrderCancellationMarkedOnAgent),
    Subtype(OrderCancelled),
    Subtype(deriveCodec[OrderAttached]),
    Subtype(deriveCodec[OrderAttachable]),
    Subtype(deriveConfiguredCodec[OrderAttachedToAgent]),
    Subtype(OrderDetachable),
    Subtype(OrderDetached),
    Subtype(deriveCodec[OrderBroken]),
    Subtype(deriveCodec[OrderLocksQueued].checked(_.checked)),
    Subtype(deriveCodec[OrderLocksAcquired].checked(_.checked)),
    Subtype(deriveCodec[OrderLocksDequeued].checked(_.checked)),
    Subtype(deriveCodec[OrderLocksReleased].checked(_.checked)),
    Subtype.decodeCompatible(deriveDecoder[OrderLockQueued])(e =>
      OrderLocksQueued(LockDemand(e.lockPath, e.count) :: Nil).checked),
    Subtype.decodeCompatible(deriveDecoder[OrderLockAcquired])(e =>
      OrderLocksAcquired(LockDemand(e.lockPath, e.count) :: Nil).checked),
    Subtype.decodeCompatible(deriveDecoder[OrderLockDequeued])(e =>
      OrderLocksDequeued(e.lockPath :: Nil).checked),
    Subtype.decodeCompatible(deriveDecoder[OrderLockReleased])(e =>
      OrderLocksReleased(e.lockPath :: Nil).checked),
    Subtype.named1[OrderNoticePosted_](typeName = "OrderNoticePosted", subclasses = Seq(
      classOf[OrderNoticePostedV2_3],
      classOf[OrderNoticePosted])),
    Subtype(deriveCodec[OrderNoticeExpected]),
    Subtype(deriveCodec[OrderNoticesExpected]),
    Subtype.singleton(OrderNoticesRead, aliases = Seq("OrderNoticeRead")),
    Subtype[OrderNoticesConsumptionStarted],
    Subtype(deriveConfiguredCodec[OrderNoticesConsumed]),
    Subtype(deriveCodec[OrderStickySubagentEntered]),
    Subtype(OrderStickySubagentLeaved),
    Subtype(deriveCodec[OrderPrompted]),
    Subtype(deriveCodec[OrderPromptAnswered]),
    Subtype(deriveCodec[OrderCyclingPrepared]),
    Subtype(OrderCycleStarted),
    Subtype(deriveCodec[OrderCycleFinished]),
    Subtype(deriveCodec[OrderTransferred]))
