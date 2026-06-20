package js7.data.order

import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils
import js7.base.circeutils.CirceUtils.{RichCirceObjectCodec, deriveCodecWithDefaults, deriveConfiguredCodec, deriveRenamingCodec}
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.{Throttle, Timestamp}
import js7.base.utils.Big
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.functionCallToString
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.agent.AgentPath
import js7.data.board.{BoardNoticeKey, BoardPath, Notice, NoticeId, NoticeKey, NoticeV2_3}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.{Event, KeyedEvent}
import js7.data.lock.LockPath
import js7.data.order.Order.*
import js7.data.order.OrderEvent.OrderMoved.Reason
import js7.data.order.OrderEvent.OrderNoticesExpected.Expected273
import js7.data.orderwatch.ExternalOrderKey
import js7.data.plan.PlanId
import js7.data.subagent.Problems.{ProcessLostDueToResetProblem, ProcessLostDueToRestartProblem, ProcessLostProblem}
import js7.data.subagent.{SubagentBundleId, SubagentId}
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.instructions.ForkBranchId
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchPath, Position, PositionOrLabel, WorkflowPosition}
import js7.data.workflow.{WorkflowId, WorkflowPath}
import org.jetbrains.annotations.TestOnly
import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event.IsKeyBase[OrderEvent]:
  val keyCompanion: OrderEvent.type = OrderEvent

  private[data] def tag: Byte


object OrderEvent extends Event.CompanionForKey[OrderId, OrderEvent]:
  implicit def implicitSelf: OrderEvent.type = this

  sealed trait IsControllerOnly extends OrderEvent

  sealed trait OrderCoreEvent extends OrderEvent
  sealed trait OrderActorEvent extends OrderCoreEvent
  sealed trait OrderTerminated extends IsControllerOnly


  sealed trait OrderAddedX extends OrderCoreEvent, OrderDetails:
    def ownOrderId: Option[OrderId]
    def workflowId: WorkflowId
    def arguments: NamedValues
    def planId: PlanId
    def scheduledFor: Option[Timestamp]
    def priority: BigDecimal
    def externalOrderKey: Option[ExternalOrderKey]
    def deleteWhenTerminated: Boolean
    def forceAdmission: Boolean
    def innerBlock: BranchPath
    def startPosition: Option[Position]
    def stopPositions: Set[PositionOrLabel]

    final def workflowPath: WorkflowPath =
      workflowId.path

  type OrderAddedEvent = OrderAdded | OrderNoticeAnnounced

  /** Chunk of events emitted when an Order is added. */
  final case class OrderAddedEvents(
    orderAdded: KeyedEvent[OrderAdded],
    noticeAnnounced: List[KeyedEvent[OrderNoticeAnnounced]]):

    def toKeyedEvents: List[KeyedEvent[OrderAddedEvent]] =
      orderAdded :: noticeAnnounced

  final case class OrderAdded(
    workflowId: WorkflowId,
    arguments: NamedValues = Map.empty,
    planId: PlanId = PlanId.Global,
    scheduledFor: Option[Timestamp] = None,
    priority: BigDecimal = Order.DefaultPriority,
    externalOrderKey: Option[ExternalOrderKey] = None,
    deleteWhenTerminated: Boolean = false,
    forceAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[Position] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  extends OrderAddedX:
    workflowId.requireNonAnonymous()

    private[data] def tag = TagOrderAdded

    def ownOrderId: None.type = None

    override def toShortString =
      s"OrderAdded(${workflowId.path})"

    override def toString =
      Vector(
        Some(workflowId),
        !planId.isGlobal ? planId,
        externalOrderKey,
        scheduledFor,
        (priority != Order.DefaultPriority) ? priority,
        forceAdmission ? "forceAdmission",
        innerBlock.nonEmpty ? innerBlock,
        startPosition,
        stopPositions.nonEmpty ? startPosition.mkString("{", ", ", "}"),
        deleteWhenTerminated ? "delete",
        arguments.nonEmpty ? s"${arguments.size} arguments",
      ).flatten.mkString("OrderAdded(", ", ", ")")


  object OrderAdded:
    private[OrderEvent] implicit val jsonCodec: Encoder.AsObject[OrderAdded] =
      o => JsonObject(
        "workflowId" -> o.workflowId.asJson,
        "scheduledFor" -> o.scheduledFor.asJson,
        "externalOrderKey" -> o.externalOrderKey.asJson,
        "arguments" -> o.arguments.??.asJson,
        "planId" -> o.planId.asJson,
        "priority" -> ((o.priority != Order.DefaultPriority) ? o.priority).asJson,
        "deleteWhenTerminated" -> o.deleteWhenTerminated.?.asJson,
        "forceAdmission" -> o.forceAdmission.?.asJson,
        "innerBlock" -> (o.innerBlock.nonEmpty ? o.innerBlock).asJson,
        "startPosition" -> o.startPosition.asJson,
        "stopPositions" -> (o.stopPositions.nonEmpty ? o.stopPositions).asJson)

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderAdded] =
      c => for
        workflowId <- c.get[WorkflowId]("workflowId")
        scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
        priority <- c.getOrElse[BigDecimal]("priority")(Order.DefaultPriority)
        externalOrderKey <- c.get[Option[ExternalOrderKey]]("externalOrderKey")
        arguments <- c.getOrElse[NamedValues]("arguments")(Map.empty)
        planId <- c.getOrElse[PlanId]("planId")(PlanId.Global/*COMPATIBLE with v2.7.3*/)
        deleteWhenTerminated <- c.getOrElse[Boolean]("deleteWhenTerminated")(false)
        forceAdmission <- c.get[Boolean]("forceJobAdmission"/*COMPATIBLE with 2.8.1*/).orElse:
          c.getOrElse[Boolean]("forceAdmission")(false)
        innerBlock <- c.getOrElse[BranchPath]("innerBlock")(BranchPath.empty)
        startPosition <- c.get[Option[Position]]("startPosition")
        stopPositions <- c.getOrElse[Set[PositionOrLabel]]("stopPositions")(Set.empty)
      yield
        OrderAdded(workflowId, arguments, planId, scheduledFor, priority, externalOrderKey,
          deleteWhenTerminated, forceAdmission,
          innerBlock, startPosition, stopPositions)


  /** Event for the AddOrder instruction. */
  final case class OrderOrderAdded(
    orderId: OrderId,
    workflowId: WorkflowId,
    arguments: NamedValues = Map.empty,
    planId: PlanId = PlanId.Global,
    priority: BigDecimal = Order.DefaultPriority,
    deleteWhenTerminated: Boolean = false,
    forceAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[Position] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty,
    speedRecord: Option[Throttle.Record] = None)
  extends OrderAddedX, OrderActorEvent:
    workflowId.requireNonAnonymous()

    private[data] def tag = TagOrderOrderAdded

    def ownOrderId: Some[OrderId] = Some(orderId)

    def scheduledFor: Option[Timestamp] = None

    def externalOrderKey: Option[ExternalOrderKey] = None

    override def toShortString = s"OrderOrderAdded($orderId, ${workflowId.path})"

  object OrderOrderAdded:
    private[OrderEvent] implicit val jsonCodec: Encoder.AsObject[OrderOrderAdded] =
      o => JsonObject(
        "orderId" -> o.orderId.asJson,
        "workflowId" -> o.workflowId.asJson,
        "arguments" -> o.arguments.??.asJson,
        "planId" -> (!o.planId.isGlobal ? o.planId).asJson,
        "priority" -> ((o.priority != Order.DefaultPriority) ? o.priority).asJson,
        "deleteWhenTerminated" -> o.deleteWhenTerminated.?.asJson,
        "forceAdmission" -> o.forceAdmission.?.asJson,
        "innerBlock" -> (o.innerBlock.nonEmpty ? o.innerBlock).asJson,
        "startPosition" -> o.startPosition.asJson,
        "stopPositions" -> o.stopPositions.??.asJson,
        "speedRecord" -> o.speedRecord.asJson)

    private[OrderEvent] implicit val jsonDecoder: Decoder[OrderOrderAdded] =
      c => for
        orderId <- c.get[OrderId]("orderId")
        workflowId <- c.get[WorkflowId]("workflowId")
        arguments <- c.getOrElse[NamedValues]("arguments")(Map.empty)
        planId <- c.getOrElse[PlanId]("planId")(PlanId.Global)
        priority <- c.getOrElse[BigDecimal]("priority")(Order.DefaultPriority)
        innerBlock <- c.getOrElse[BranchPath]("innerBlock")(BranchPath.empty)
        startPosition <- c.get[Option[Position]]("startPosition")
        stopPositions <- c.getOrElse[Set[PositionOrLabel]]("stopPositions")(Set.empty)
        deleteWhenTerminated <- c.getOrElse[Boolean]("deleteWhenTerminated")(false)
        forceAdmission <- c.get[Boolean]("forceJobAdmission"/*COMPATIBLE with 2.8.1*/).orElse:
          c.getOrElse[Boolean]("forceAdmission")(false)
        speedRecord <- c.get[Option[Throttle.Record]]("speedRecord")
      yield
        OrderOrderAdded(orderId, workflowId, arguments, planId, priority,
          deleteWhenTerminated, forceAdmission,
          innerBlock, startPosition, stopPositions, speedRecord)


  /** Agent-only event. */
  final case class OrderAttachedToAgent(
    workflowPosition: WorkflowPosition,
    state: IsFreshOrReady,
    planId: PlanId = PlanId.Global,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    externalOrder: Option[ExternalOrderLink] = None,
    historicOutcomes: Vector[HistoricOutcome] = Vector.empty,
    agentPath: AgentPath,
    parent: Option[OrderId] = None,
    priority: Option[BigDecimal] = None,
    mark: Option[OrderMark] = None,
    isSuspended: Boolean = false,
    isResumed: Boolean = false,
    deleteWhenTerminated: Boolean = false,
    forceAdmission: Boolean = false,
    stickySubagents: List[Order.StickySubagent] = Nil,
    innerBlock: BranchPath = BranchPath.empty,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  extends OrderCoreEvent:
    workflowPosition.workflowId.requireNonAnonymous()

    private[data] def tag = TagOrderAttachedToAgent

    def workflowId: WorkflowId = workflowPosition.workflowId

    override def hasShortString = true

    override def toShortString = "OrderAttachedToAgent"

  object OrderAttachedToAgent:
    given Codec.AsObject[OrderAttachedToAgent] = deriveRenamingCodec(Map(
      "externalOrderKey" -> "externalOrder",
      "forceJobAdmission" -> "forceAdmission"))

  final case class OrderAttached(agentPath: AgentPath)
  extends OrderCoreEvent:
    private[data] def tag = TagOrderAttached


  type OrderStarted = OrderStarted.type
  case object OrderStarted extends OrderActorEvent:
    private[data] def tag = TagOrderStarted


  // subagentId = None is COMPATIBLE with v2.2
  final case class OrderProcessingStarted(
    subagentId: Option[SubagentId],
    subagentBundleId: Option[SubagentBundleId] = None,
    stick: Boolean = false,
    endOfAdmissionPeriod: Option[Timestamp] = None)
  extends OrderCoreEvent:
    private[data] def tag = TagOrderProcessingStarted

    override def toString =
      s"OrderProcessingStarted(${Array(subagentId, subagentBundleId, stick ? "stick").flatten.mkString(" ")})"

  object OrderProcessingStarted:
    val noSubagent: OrderProcessingStarted = new OrderProcessingStarted(None, None, false)
    // Since v2.3
    @TestOnly
    def apply(subagentId: SubagentId): OrderProcessingStarted =
      new OrderProcessingStarted(Some(subagentId), None, false)

    // Since v2.3
    @TestOnly
    def apply(subagentId: SubagentId, stick: Boolean, endOfAdmissionPeriod: Option[Timestamp])
    : OrderProcessingStarted =
      new OrderProcessingStarted(Some(subagentId), None, stick, endOfAdmissionPeriod)


  sealed trait OrderStdWritten extends OrderEvent:
    override def hasShortString = true
    def stdoutStderr: StdoutOrStderr
    def chunk: String

    override def toString: String = getClass.simpleScalaName + "(" +
      chunk.truncateWithEllipsis(
        300/*for error lines with long (file path) prefix*/,
        showLength = true) + ")"

  object OrderStdWritten:
    def apply(t: StdoutOrStderr): String => OrderStdWritten =
      t match
        case Stdout => OrderStdoutWritten.apply
        case Stderr => OrderStderrWritten.apply

    def unapply(o: OrderStdWritten): Some[(StdoutOrStderr, String)] = o match
      case OrderStdoutWritten(chunk) => Some((Stdout, chunk))
      case OrderStderrWritten(chunk) => Some((Stderr, chunk))


  final case class OrderStdoutWritten(chunk: String) extends OrderStdWritten:
    private[data] def tag = TagOrderStdoutWritten
    def stdoutStderr: StdoutOrStderr = Stdout
    override def toString: String = super.toString

  object OrderStdoutWritten:
    implicit val jsonCodec: Codec.AsObject[OrderStdoutWritten] = deriveCodec[OrderStdoutWritten]


  final case class OrderStderrWritten(chunk: String) extends OrderStdWritten:
    private[data] def tag = TagOrderStderrWritten
    def stdoutStderr: StdoutOrStderr = Stderr
    override def toString: String = super.toString

  object OrderStderrWritten:
    implicit val jsonCodec: Codec.AsObject[OrderStderrWritten] = deriveCodec[OrderStderrWritten]


  final case class OrderProcessed(outcome: OrderOutcome) extends OrderCoreEvent:
    private[data] def tag = TagOrderProcessed
    override def isSucceeded: Boolean = outcome.isSucceeded

  object OrderProcessed:
    @TestOnly
    val processLostDueToRestart: OrderProcessed = processLost(ProcessLostDueToRestartProblem)
    @TestOnly
    val processLostDueToReset: OrderProcessed = processLost(ProcessLostDueToResetProblem)

    /** ProcessLost may restart the Execute instruction. Only for temporary problems! */
    def processLost(problem: ProcessLostProblem): OrderProcessed =
      OrderProcessed(OrderOutcome.processLost(problem))

    /** ProcessLost may restart the Execute instruction. Only for temporary problems! */
    def processLostUnchecked(problem: Problem): OrderProcessed =
      OrderProcessed(OrderOutcome.processLostUnchecked(problem))

    implicit val jsonCodec: Codec.AsObject[OrderProcessed] = deriveCodec[OrderProcessed]


  final case class OrderForked(children: Vector[OrderForked.Child]) extends OrderActorEvent:
    private[data] def tag = TagOrderForked


  object OrderForked:
    final case class Child(
      orderId: OrderId,
      arguments: Map[String, Value],
      branchId: Option[ForkBranchId] = None):

      override def toString =
        s"${branchId.fold("")(_.string + ": ")}$orderId${
          if arguments.isEmpty then
            ""
          else
            " " + arguments.map(o => s"${o._1}=${o._2}").mkString("{", ", ", "}")
        }"

    object Child:
      def apply(branchId: ForkBranchId, orderId: OrderId): Child =
        new Child(orderId, Map.empty, Some(branchId))

      implicit def fromPair(pair: (String, OrderId)): Child =
        apply(ForkBranchId(pair._1), pair._2)

      implicit val jsonEncoder: Encoder.AsObject[Child] =
        o => JsonObject(
          "orderId" -> o.orderId.asJson,
          "arguments" -> o.arguments.??.asJson,
          "branchId" -> o.branchId.asJson)

      implicit val jsonDecoder: Decoder[Child] =
        c => for
          orderId <- c.get[OrderId]("orderId")
          arguments <- c.getOrElse[Map[String, Value]]("arguments")(Map.empty)
          branchId <- c.get[Option[ForkBranchId]]("branchId")
        yield Child(orderId, arguments, branchId)


  final case class OrderJoined(outcome: OrderOutcome)
  extends OrderActorEvent:
    private[data] def tag = TagOrderJoined
    override def isSucceeded: Boolean = outcome.isSucceeded


  sealed trait OrderNoticeEvent extends OrderActorEvent


  final case class OrderNoticeAnnounced(noticeId: NoticeId)
  extends OrderNoticeEvent:
    private[data] def tag = TagOrderNoticeAnnounced

  object OrderNoticeAnnounced:
    given Codec.AsObject[OrderNoticeAnnounced] = deriveCodec


  sealed trait OrderNoticePosted_ extends OrderNoticeEvent

  object OrderNoticePosted_ :
    given Encoder.AsObject[OrderNoticePosted_] =
      case o: OrderNoticePostedV2_3 => OrderNoticePostedV2_3.jsonEncoder.encodeObject(o)
      case o: OrderNoticePosted => OrderNoticePosted.jsonCodec.encodeObject(o)

    given Decoder[OrderNoticePosted_] = c =>
      if c.get[Json]("noticeId").isRight then
        OrderNoticePosted.jsonCodec(c)
      else if c.value.asObject.flatMap(_("notice")).flatMap(_.asObject)
        .flatMap(_("boardPath")).exists(_.isString)
      then
        for
          notice <- c.get[Notice]("notice")
          _ <-
            if notice.planId == PlanId.Global /*The Order's PlanId is used*/ then
              Right(())
            else
              Left(DecodingFailure("OrderNoticePosted must not contain a PlanId", c.history))
        yield
          OrderNoticePosted(notice.id, notice.endOfLife)
      else
        c.get[NoticeV2_3]("notice").map(OrderNoticePostedV2_3(_))


  // COMPATIBLE with v2.3
  final case class OrderNoticePostedV2_3(notice: NoticeV2_3)
  extends OrderNoticePosted_ :
    private[data] def tag = TagOrderNoticePostedV2_3

  object OrderNoticePostedV2_3:
    implicit val jsonEncoder: Encoder.AsObject[OrderNoticePostedV2_3] = deriveEncoder


  final case class OrderNoticePosted(
    noticeId: NoticeId,
    endOfLife: Option[Timestamp] = None)
  extends OrderNoticePosted_ :
    private[data] def tag = TagOrderNoticePosted

  object OrderNoticePosted:
    private[OrderEvent] val jsonCodec: Codec.AsObject[OrderNoticePosted] = deriveCodecWithDefaults


  // COMPATIBLE with v2.3
  final case class OrderNoticeExpected(private val noticeId: NoticeKey)
  extends OrderNoticeEvent:
    private[data] def tag = TagOrderNoticeExpected
    def noticeKey: NoticeKey = noticeId


  final case class OrderNoticesExpected(noticeIds: Vector[NoticeId])
  extends OrderNoticeEvent:
    private[data] def tag = TagOrderNoticesExpected

  object OrderNoticesExpected:
    private val jsonCodec: Codec.AsObject[OrderNoticesExpected] = deriveCodec

    given Encoder.AsObject[OrderNoticesExpected] = jsonCodec

    @nowarn("msg=class Expected273 in object OrderNoticesExpected is deprecated since v2.7.4")
    given Decoder[OrderNoticesExpected] = c =>
      if c.downField("expected").succeeded then
        for
          expected <- c.get[Vector[Expected273]]("expected")
        yield
          OrderNoticesExpected(expected.map(PlanId.Global / _.boardNoticeKey))
      else
        jsonCodec(c)

    @deprecated("use BoardNoticeKey", "v2.7.4")
    final case class Expected273(boardNoticeKey: BoardNoticeKey)

    @nowarn("msg=class Expected273 in object OrderNoticesExpected is deprecated since v2.7.4")
    object Expected273:
      given Decoder[Expected273] = c =>
        for
          boardPath <- c.get[BoardPath]("boardPath")
          noticeKey <- c.get[NoticeKey]("noticeId")
        yield
          Expected273(boardPath / noticeKey)

  type OrderNoticesRead = OrderNoticesRead.type
  case object OrderNoticesRead
  extends OrderNoticeEvent:
    private[data] def tag = TagOrderNoticesRead


  final case class OrderNoticesConsumptionStarted(noticeIds: Vector[NoticeId])
  extends OrderNoticeEvent:
    private[data] def tag = TagOrderNoticesConsumptionStarted
    def checked: Checked[this.type] =
      noticeIds.checkUniquenessBy(_.boardPath).rightAs(this)

  object OrderNoticesConsumptionStarted:
    private val jsonCodec: Codec.AsObject[OrderNoticesConsumptionStarted] =
      deriveCodec[OrderNoticesConsumptionStarted].checked(_.checked)

    given Encoder.AsObject[OrderNoticesConsumptionStarted] = jsonCodec

    @nowarn("msg=class Expected273 in object OrderNoticesExpected is deprecated since v2.7.4")
    given Decoder[OrderNoticesConsumptionStarted] = c =>
      if c.downField("consumptions").succeeded then
        for
          expected <- c.get[Vector[Expected273]]("consumptions")
        yield
          OrderNoticesConsumptionStarted(expected.map(PlanId.Global / _.boardNoticeKey))
      else
        jsonCodec(c)


  final case class OrderNoticesConsumed(failed: Boolean = false)
  extends OrderNoticeEvent:
    private[data] def tag = TagOrderNoticesConsumed

  // Similar to OrderProcessingCancelled, but only for the Processing state.
  // Maybe we want to differentiate between cancellation of an order and killing of a process.
  type OrderProcessingKilled = OrderProcessingKilled.type
  case object OrderProcessingKilled
  extends OrderActorEvent:
    private[data] def tag = TagOrderProcessingKilled


  final case class OrderStickySubagentEntered(
    agentPath: AgentPath,
    subagentBundleId: Option[SubagentBundleId] = None)
  extends OrderActorEvent:
    private[data] def tag = TagOrderStickySubagentEntered

  object OrderStickySubagentEntered:
    given Codec.AsObject[OrderStickySubagentEntered] = deriveRenamingCodec(Map(
      "subagentSelectionId" -> "subagentBundleId"))


  type OrderStickySubagentLeaved = OrderStickySubagentLeaved.type
  case object OrderStickySubagentLeaved
  extends OrderActorEvent:
    private[data] def tag = TagOrderStickySubagentLeaved


  final case class OrderMoved(to: Position, reason: Option[Reason] = None)
  extends OrderActorEvent:
    private[data] def tag = TagOrderMoved
    override def toString = s"OrderMoved($to${reason.fold("")(" " + _)})"
  object OrderMoved:
    sealed trait Reason
    case object SkippedDueToWorkflowPathControl extends Reason
    case object NoNotice extends Reason
    case object NoAdmissionPeriodStart extends Reason

    implicit val jsonCodec: TypedJsonCodec[Reason] = TypedJsonCodec(
      Subtype(SkippedDueToWorkflowPathControl),
      Subtype(NoNotice),
      Subtype(NoAdmissionPeriodStart))


  sealed trait OrderFailedEvent extends OrderActorEvent:
    def moveTo(movedTo: Position): OrderFailedEvent

    def movedTo: Position


  final case class OrderOutcomeAdded(outcome: OrderOutcome)
  extends OrderActorEvent:
    private[data] def tag = TagOrderOutcomeAdded


  final case class OrderFailed(
    movedTo: Position,
    // COMPATIBLE with v2.4: outcome has been replaced by OrderOutcomeAdded event
    outcome: Option[OrderOutcome.NotSucceeded])
  extends OrderFailedEvent, OrderTerminated:
    private[data] def tag = TagOrderFailed
    def moveTo(movedTo: Position): OrderFailedEvent = copy(movedTo = movedTo)


  object OrderFailed:
    @deprecated("outcome is deprecated", "v2.5")
    private[order] def apply(movedTo: Position, outcome: Option[OrderOutcome.NotSucceeded]) =
      new OrderFailed(movedTo, outcome)

    def apply(movedTo: Position) = new OrderFailed(movedTo, None)


  final case class OrderFailedInFork(
    movedTo: Position,
    // outcome has been replaced by OrderOutcomeAdded event. COMPATIBLE with v2.4
    outcome: Option[OrderOutcome.NotSucceeded] = None)
  extends OrderFailedEvent, IsControllerOnly:
    private[data] def tag = TagOrderFailedInFork
    def moveTo(movedTo: Position): OrderFailedEvent = copy(movedTo = movedTo)

  object OrderFailedInFork:
    @deprecated("outcome is deprecated", "v2.5")
    private[order] def apply(movedTo: Position, outcome: Option[OrderOutcome.NotSucceeded]) =
      new OrderFailedInFork(movedTo, outcome)

    def apply(movedTo: Position) = new OrderFailedInFork(movedTo, None)


  // COMPATIBLE with v2.4
  final case class OrderCatched(
    movedTo: Position,
    outcome: Option[OrderOutcome.NotSucceeded] = None)
  extends OrderFailedEvent:
    private[data] def tag = TagOrderCatched
    def moveTo(movedTo: Position): OrderFailedEvent = copy(movedTo = movedTo)


  final case class OrderCaught(
    movedTo: Position,
    // COMPATIBLE with v2.4: outcome has been replaced by OrderOutcomeAdded event
    outcome: Option[OrderOutcome.NotSucceeded] = None)
  extends OrderFailedEvent:
    private[data] def tag = TagOrderCaught
    def moveTo(movedTo: Position): OrderFailedEvent = copy(movedTo = movedTo)

  object OrderCaught:
    @deprecated("outcome is deprecated", "v2.5")
    private[order] def apply(movedTo: Position, outcome: Option[OrderOutcome.NotSucceeded]) =
      new OrderCaught(movedTo, outcome)

    def apply(movedTo: Position) = new OrderCaught(movedTo, None)


  final case class OrderRetrying(
    delayedUntil: Option[Timestamp] = None,
    movedTo: Option[Position] = None /*COMPATIBLE with v2.7.1*/)
  extends OrderActorEvent:
    private[data] def tag = TagOrderRetrying
    override def toString =
      if delayedUntil.isEmpty && movedTo.isEmpty then
        "OrderRetrying"
      else
        (delayedUntil.map(_.toString) ++ movedTo)
          .mkString("OrderRetrying(", " ", ")")


  type OrderAwoke = OrderAwoke.type
  case object OrderAwoke extends OrderActorEvent:
    private[data] def tag = TagOrderAwoke


  final case class OrderBroken(problem: Option[Problem]) extends OrderActorEvent:
    private[data] def tag = TagOrderBroken
    override def toString = s"💥 OrderBroken($problem)"

  object OrderBroken:
    // COMPATIBLE with v2.4
    @deprecated("outcome is deprecated", "v2.5")
    private[order] def apply(problem: Problem): OrderBroken =
      OrderBroken(Some(problem))

    def apply(): OrderBroken =
      OrderBroken(None)


  /**
    * Controller may have started to attach Order to Agent.
    */
  final case class OrderAttachable(agentPath: AgentPath) extends OrderActorEvent:
    private[data] def tag = TagOrderAttachable


  type OrderDetachable = OrderDetachable.type
  /**
    * Agent has processed all steps and the Order should be fetched by the Controller.
    */
  case object OrderDetachable extends OrderActorEvent:
    private[data] def tag = TagOrderDetachable


  type OrderDetached = OrderDetached.type
  /**
    * Order has been removed from the Agent and is held by the Controller.
    * Agent-only event.
    */
  case object OrderDetached extends OrderCoreEvent:
    private[data] def tag = TagOrderDetached


  final case class OrderFinished(outcome: Option[OrderOutcome.Completed] = None)
  extends OrderActorEvent, OrderTerminated:
    private[data] def tag = TagOrderFinished
    override def toString = functionCallToString("OrderFinished", outcome)


  type OrderExternalVanished = OrderExternalVanished.type
  case object OrderExternalVanished extends OrderActorEvent:
    private[data] def tag = TagOrderExternalVanished


  type OrderDeletionMarked = OrderDeletionMarked.type
  case object OrderDeletionMarked extends OrderActorEvent:
    private[data] def tag = TagOrderDeletionMarked


  type OrderDeleted = OrderDeleted.type
  case object OrderDeleted extends OrderActorEvent:
    private[data] def tag = TagOrderDeleted


  sealed trait OrderKillingMarked extends OrderActorEvent:
    def kill: Option[CancellationMode.Kill]

  object OrderKillingMarked:
    def unapply(event: OrderKillingMarked): Some[Option[CancellationMode.Kill]] = Some(event.kill)

  /** A OrderCancellationMarked on Agent is different from same Event at the Controller.
    * Controller will ignore the Agent's OrderCancellationMarked.
    * Controller should have emitted the event independendly.
    **/
  final case class OrderCancellationMarked(mode: CancellationMode = CancellationMode.Default)
  extends OrderKillingMarked:
    private[data] def tag = TagOrderCancellationMarked

    def kill: Option[CancellationMode.Kill] = mode match
      case CancellationMode.FreshOrStarted(kill) => kill
      case _ => None

    override def toString =
      functionCallToString("OrderCancellationMarked", (mode != CancellationMode.Default) ? mode)


  type OrderCancellationMarkedOnAgent = OrderCancellationMarkedOnAgent.type
  /** No other use than notifying an external user. */
  case object OrderCancellationMarkedOnAgent extends OrderActorEvent:
    private[data] def tag = TagOrderCancellationMarkedOnAgent


  type OrderStateReset = OrderStateReset.type
  case object OrderStateReset extends OrderActorEvent:
    private[data] def tag = TagOrderStateReset


  type OrderCancelled = OrderCancelled.type
  case object OrderCancelled extends OrderActorEvent, OrderTerminated:
    private[data] def tag = TagOrderCancelled


  final case class OrderSuspensionMarked(mode: SuspensionMode = SuspensionMode.standard)
  extends OrderKillingMarked:
    private[data] def tag = TagOrderSuspensionMarked
    def kill: Option[CancellationMode.Kill] = mode.kill


  final case class OrderGoMarked(position: Position)
  extends OrderActorEvent:
    private[data] def tag = TagOrderGoMarked


  type OrderGoes = OrderGoes.type
  case object OrderGoes
  extends OrderActorEvent:
    private[data] def tag = TagOrderGoes


  type OrderSuspensionMarkedOnAgent = OrderSuspensionMarkedOnAgent.type
  /** No other use than notifying an external user. */
  case object OrderSuspensionMarkedOnAgent extends OrderActorEvent:
    private[data] def tag = TagOrderSuspensionMarkedOnAgent


  type OrderSuspended = OrderSuspended.type
  case object OrderSuspended extends OrderActorEvent, IsControllerOnly:
    private[data] def tag = TagOrderSuspended


  final case class OrderPriorityChanged(priority: BigDecimal)
  extends OrderActorEvent, IsControllerOnly/*for now*/:
    private[data] def tag = TagOrderPriorityChanged

  type OrderStopped = OrderStopped.type
  case object OrderStopped extends OrderActorEvent, IsControllerOnly:
    private[data] def tag = TagOrderStopped


  final case class OrderResumptionMarked(
    position: Option[Position] = None,
    historyOperations: Seq[OrderResumed.HistoryOperation] = Nil,
    asSucceeded: Boolean = false,
    restartKilledJob: Boolean = false)
  extends OrderActorEvent, Big:
    private[data] def tag = TagOrderResumptionMarked


  final case class OrderResumed(
    position: Option[Position] = None,
    historyOperations: Seq[OrderResumed.HistoryOperation] = Nil,
    asSucceeded: Boolean = false,
    restartKilledJob: Boolean = false)
  extends OrderActorEvent, Big:
    private[data] def tag = TagOrderResumed
    override def toString =
      functionCallToString("OrderResumed", position.view, historyOperations,
        asSucceeded ? "asSucceeded",
        restartKilledJob ? "restartKilledJob")

  object OrderResumed:
    sealed trait HistoryOperation:
      def positions: Iterable[Position]

    final case class ReplaceHistoricOutcome(position: Position, outcome: OrderOutcome)
    extends HistoryOperation:
      def positions: Iterable[Position] = position :: Nil
    object ReplaceHistoricOutcome:
      def apply(historicOutcome: HistoricOutcome) =
        new ReplaceHistoricOutcome(historicOutcome.position, historicOutcome.outcome)

    final case class InsertHistoricOutcome(before: Position, position: Position, outcome: OrderOutcome)
    extends HistoryOperation:
      def positions: Iterable[Position] = before :: position :: Nil
    object InsertHistoricOutcome:
      def apply(at: Position, historicOutcome: HistoricOutcome) =
        new InsertHistoricOutcome(at, historicOutcome.position, historicOutcome.outcome)

    final case class DeleteHistoricOutcome(position: Position)
    extends HistoryOperation:
      def positions: Iterable[Position] = position :: Nil

    final case class AppendHistoricOutcome(position: Position, outcome: OrderOutcome)
    extends HistoryOperation:
      def positions: Iterable[Position] = position :: Nil
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
  final case class OrderLockAcquired(lockPath: LockPath, count: Option[Int])
  extends LegacyOrderLockEvent:
    private[data] def tag = TagOrderLockAcquired


  // COMPATIBLE with v2.4
  final case class OrderLockQueued(lockPath: LockPath, count: Option[Int])
  extends LegacyOrderLockEvent:
    private[data] def tag = TagOrderLockQueued


  // COMPATIBLE with v2.4
  final case class OrderLockDequeued(lockPath: LockPath)
  extends LegacyOrderLockEvent:
    private[data] def tag = TagOrderLockDequeued


  // COMPATIBLE with v2.4
  final case class OrderLockReleased(lockPath: LockPath)
  extends LegacyOrderLockEvent:
    private[data] def tag = TagOrderLockReleased


  sealed trait OrderLockEvent extends OrderActorEvent:
    def lockPaths: Seq[LockPath]


  final case class OrderLocksQueued(demands: List[LockDemand])
  extends OrderLockEvent:
    private[data] def tag = TagOrderLocksQueued
    def checked: Checked[OrderLocksQueued] = LockDemand.checked(demands).rightAs(this)
    def lockPaths: Seq[LockPath] = demands.map(_.lockPath)


  final case class OrderLocksAcquired(demands: List[LockDemand])
  extends OrderLockEvent:
    private[data] def tag = TagOrderLocksAcquired
    def checked: Checked[OrderLocksAcquired] = LockDemand.checked(demands).rightAs(this)
    def lockPaths: Seq[LockPath] = demands.map(_.lockPath)


  final case class OrderLocksReleased(lockPaths: Seq[LockPath])
  extends OrderLockEvent:
    private[data] def tag = TagOrderLocksReleased
    def checked: Checked[OrderLocksReleased] = lockPaths.checkUniqueness.rightAs(this)


  final case class LockDemand(lockPath: LockPath, count: Option[Int] = None):
    def checked: Checked[this.type] =
      if count.exists(_ < 1) then
        Left(Problem(s"LockDemand.count must not be below 1 for $lockPath"))
      else
        Right(this)

  object LockDemand:
    def checked(demands: Seq[LockDemand]): Checked[Unit] =
      demands
        .checkUniquenessBy(_.lockPath)
        .>>(demands.traverse(_.checked))
        .rightAs(())

    implicit val jsonCodec: Codec.AsObject[LockDemand] = deriveCodec


  final case class OrderPrompted(question: Value)
  extends OrderActorEvent:
    private[data] def tag = TagOrderPrompted


  final case class OrderPromptAnswered(/*outcome: OrderOutcome.Completed*/)
  extends OrderCoreEvent, IsControllerOnly:
    private[data] def tag = TagOrderPromptAnswered


  sealed trait OrderCycleEvent extends OrderActorEvent

  final case class OrderCyclingPrepared(cycleState: CycleState)
  extends OrderCycleEvent:
    private[data] def tag = TagOrderCyclingPrepared


  /**
    * @param skipped Only when Ticking ticks have been skipped.
    *                Must be a multiple of the tick interval,
    *                will be added to `next`.
    */
  final case class OrderCycleStarted(skipped: Option[FiniteDuration] = None)
  extends OrderCycleEvent:
    private[data] def tag = TagOrderCycleStarted
    override def toString = s"OrderCycleStarted${skipped.fold("")(o => s"(skipped=${o.pretty})")}"


  final case class OrderCycleFinished(cycleState: Option[CycleState])
  extends OrderCycleEvent:
    private[data] def tag = TagOrderCycleFinished


  final case class OrderSleeping(until: Timestamp, cause: OrderSleeping.Cause)
  extends OrderActorEvent:
    private[data] def tag = TagOrderSleeping

    override def toString =
      val elapsed = Timestamp.now - until
      val elapsedString =
        if elapsed.isNegative && elapsed >= -OrderSleeping.LogMax then
          s" -${(-elapsed).pretty}"
        else if elapsed.isPositive && elapsed <= OrderSleeping.LogMax then
          s" +${elapsed.pretty}"
        else ""
      s"OrderSleeping($until$elapsedString, $cause)"

  object OrderSleeping:
    private val LogMax = 1.h.toNanos.ns

    sealed trait Cause

    object Cause:
      case object SleepInstruction extends Cause
      case object Throttle extends Cause

      given TypedJsonCodec[Cause] = TypedJsonCodec(
        Subtype(SleepInstruction),
        Subtype(Throttle))

    given Encoder.AsObject[OrderSleeping] = deriveEncoder
    given Decoder[OrderSleeping] = c =>
      for
        until <- c.get[Timestamp]("until")
        cause <- c.getOrElse[OrderSleeping.Cause]("cause")(Cause.SleepInstruction)
      yield
        OrderSleeping(until, cause)


  final case class OrderWaitingForAdmission(until: Timestamp)
  extends OrderActorEvent:
    private[data] def tag = TagOrderWaitingForAdmission


  final case class OrderTransferred(workflowPosition: WorkflowPosition)
  extends OrderActorEvent:
    private[data] def tag = TagOrderTransferred


  final case class OrderPlanAttached(planId: PlanId)
  extends OrderActorEvent:
    private[data] def tag = TagOrderPlanAttached


  /** TEST ONLY — EXPERIMENTAL */
  final case class OrderSaid(value: Value)
  extends OrderActorEvent:
    private[data] def tag = TagOrderSaid


  private[data] inline val TagOrderStdoutWritten = 1
  private[data] inline val TagOrderStderrWritten = 2
  private[data] inline val TagOrderAdded = 3
  private[data] inline val TagOrderOrderAdded = 4
  private[data] inline val TagOrderAttachedToAgent = 5
  private[data] inline val TagOrderAttached = 6
  private[data] inline val TagOrderStarted = 7
  private[data] inline val TagOrderProcessingStarted = 8
  private[data] inline val TagOrderProcessed = 9
  private[data] inline val TagOrderForked = 10
  private[data] inline val TagOrderJoined = 11
  private[data] inline val TagOrderNoticeAnnounced = 12
  private[data] inline val TagOrderNoticePostedV2_3 = 13
  private[data] inline val TagOrderNoticePosted = 14
  private[data] inline val TagOrderNoticeExpected = 15
  private[data] inline val TagOrderNoticesExpected = 16
  private[data] inline val TagOrderNoticesRead = 17
  private[data] inline val TagOrderNoticesConsumptionStarted = 18
  private[data] inline val TagOrderNoticesConsumed = 19
  private[data] inline val TagOrderProcessingKilled = 20
  private[data] inline val TagOrderStickySubagentEntered = 21
  private[data] inline val TagOrderStickySubagentLeaved = 22
  private[data] inline val TagOrderMoved = 23
  private[data] inline val TagOrderOutcomeAdded = 24
  private[data] inline val TagOrderFailed = 25
  private[data] inline val TagOrderFailedInFork = 26
  private[data] inline val TagOrderCatched = 27
  private[data] inline val TagOrderCaught = 28
  private[data] inline val TagOrderRetrying = 29
  private[data] inline val TagOrderAwoke = 30
  private[data] inline val TagOrderBroken = 31
  private[data] inline val TagOrderAttachable = 32
  private[data] inline val TagOrderDetachable = 33
  private[data] inline val TagOrderDetached = 34
  private[data] inline val TagOrderFinished = 35
  private[data] inline val TagOrderExternalVanished = 36
  private[data] inline val TagOrderDeletionMarked = 37
  private[data] inline val TagOrderDeleted = 38
  private[data] inline val TagOrderCancellationMarked = 39
  private[data] inline val TagOrderCancellationMarkedOnAgent = 40
  private[data] inline val TagOrderStateReset = 41
  private[data] inline val TagOrderCancelled = 42
  private[data] inline val TagOrderSuspensionMarked = 43
  private[data] inline val TagOrderGoMarked = 44
  private[data] inline val TagOrderGoes = 45
  private[data] inline val TagOrderSuspensionMarkedOnAgent = 46
  private[data] inline val TagOrderSuspended = 47
  private[data] inline val TagOrderPriorityChanged = 48
  private[data] inline val TagOrderStopped = 49
  private[data] inline val TagOrderResumptionMarked = 50
  private[data] inline val TagOrderResumed = 51
  private[data] inline val TagOrderLockAcquired = 52
  private[data] inline val TagOrderLockQueued = 53
  private[data] inline val TagOrderLockDequeued = 54
  private[data] inline val TagOrderLockReleased = 55
  private[data] inline val TagOrderLocksQueued = 56
  private[data] inline val TagOrderLocksAcquired = 57
  private[data] inline val TagOrderLocksReleased = 58
  //private[data] inline val Tag… = 59
  private[data] inline val TagOrderPrompted = 60
  private[data] inline val TagOrderPromptAnswered = 61
  private[data] inline val TagOrderCyclingPrepared = 62
  private[data] inline val TagOrderCycleStarted = 63
  private[data] inline val TagOrderCycleFinished = 64
  private[data] inline val TagOrderSleeping = 65
  private[data] inline val TagOrderWaitingForAdmission = 66
  private[data] inline val TagOrderTransferred = 67
  private[data] inline val TagOrderPlanAttached = 68
  private[data] inline val TagOrderSaid = 69

  @nowarn("msg=deprecated")
  implicit val jsonCodec: TypedJsonCodec[OrderEvent] = TypedJsonCodec(
    Subtype[OrderAdded],
    Subtype[OrderOrderAdded],
    Subtype(OrderExternalVanished),
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
    Subtype(deriveCodec[OrderPriorityChanged]),
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
    Subtype.singleton(OrderStateReset, aliases = Seq(
      "OrderOperationCancelled", "OrderLocksDequeued", "OrderLockDequeued")),
    Subtype(OrderCancellationMarkedOnAgent),
    Subtype(OrderCancelled),
    Subtype(deriveCodec[OrderAttached]),
    Subtype(deriveCodec[OrderAttachable]),
    Subtype[OrderAttachedToAgent],
    Subtype(OrderDetachable),
    Subtype(OrderDetached),
    Subtype(deriveCodec[OrderBroken]),
    Subtype(deriveCodec[OrderLocksQueued].checked(_.checked)),
    Subtype(deriveCodec[OrderLocksAcquired].checked(_.checked)),
    Subtype(deriveCodec[OrderLocksReleased].checked(_.checked)),
    Subtype.decodeCompatible(deriveDecoder[OrderLockQueued])(e =>
      OrderLocksQueued(LockDemand(e.lockPath, e.count) :: Nil).checked),
    Subtype.decodeCompatible(deriveDecoder[OrderLockAcquired])(e =>
      OrderLocksAcquired(LockDemand(e.lockPath, e.count) :: Nil).checked),
    Subtype.decodeCompatible(deriveDecoder[OrderLockReleased])(e =>
      OrderLocksReleased(e.lockPath :: Nil).checked),
    Subtype[OrderNoticeAnnounced],
    Subtype.named1[OrderNoticePosted_](typeName = "OrderNoticePosted", subclasses = Seq(
      classOf[OrderNoticePostedV2_3],
      classOf[OrderNoticePosted])),
    Subtype(deriveCodec[OrderNoticeExpected]),
    Subtype[OrderNoticesExpected],
    Subtype.singleton(OrderNoticesRead, aliases = Seq("OrderNoticeRead")),
    Subtype[OrderNoticesConsumptionStarted],
    Subtype(deriveConfiguredCodec[OrderNoticesConsumed]),
    Subtype[OrderStickySubagentEntered],
    Subtype(OrderStickySubagentLeaved),
    Subtype(deriveCodec[OrderPrompted]),
    Subtype(deriveCodec[OrderPromptAnswered]),
    Subtype(deriveCodec[OrderCyclingPrepared]),
    Subtype(deriveCodecWithDefaults[OrderCycleStarted]),
    Subtype(deriveCodec[OrderCycleFinished]),
    Subtype[OrderSleeping],
    Subtype(deriveCodec[OrderTransferred]),
    Subtype(deriveCodec[OrderPlanAttached]),
    Subtype(deriveCodec[OrderSaid]),
    Subtype(deriveCodec[OrderWaitingForAdmission]))
