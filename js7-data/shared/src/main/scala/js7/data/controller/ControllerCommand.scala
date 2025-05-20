package js7.data.controller

import io.circe.derivation.ConfiguredCodec
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils.{deriveCodecWithDefaults, deriveConfiguredCodec}
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.log.CorrelIdWrapped
import js7.base.problem.Checked
import js7.base.problem.Checked.implicits.{checkedJsonDecoder, checkedJsonEncoder}
import js7.base.time.Timestamp
import js7.base.utils.Big
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, GlobalBoard, NoticeId, NoticeKey, PlannableBoard}
import js7.data.command.{CancellationMode, CommonCommand, SuspensionMode}
import js7.data.controller.ControllerState.*
import js7.data.event.EventId
import js7.data.node.NodeId
import js7.data.order.OrderEvent.OrderResumed
import js7.data.order.{FreshOrder, OrderId}
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId, PlanStatus}
import js7.data.subagent.SubagentId
import js7.data.value.expression.{ExprFunction, Scope}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.position.{Label, Position}
import js7.data.workflow.{WorkflowId, WorkflowPath}
import scala.collection.immutable
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait ControllerCommand extends CommonCommand:
  type Response <: ControllerCommand.Response


object ControllerCommand extends CommonCommand.Companion:
  protected type Command = ControllerCommand

  final case class Batch(commands: Seq[CorrelIdWrapped[ControllerCommand]])
  extends ControllerCommand, CommonBatch, Big:
    type Response = Batch.Response
  object Batch:
    final case class Response(responses: Seq[Checked[ControllerCommand.Response]])
    extends ControllerCommand.Response, CommonBatch.Response, Big:
      override def productPrefix = "BatchResponse"

  final case class AddOrder(order: FreshOrder) extends ControllerCommand:
    type Response = AddOrder.Response
    override def toShortString = s"AddOrder(${order.id}, ${order.workflowPath})"
  object AddOrder:
    final case class Response(ignoredBecauseDuplicate: Boolean)
    extends ControllerCommand.Response

  final case class AddOrders(orders: Seq[FreshOrder]) extends ControllerCommand:
    type Response = AddOrders.Response
    override def toShortString = s"AddOrders(${orders.size} orders, ${orders.take(1).map(o => o.toString.truncateWithEllipsis(200) + ", ").mkString} ...)"
  object AddOrders:
    // AddOrderResponse is unnested to be accessible for Java code
    type Response = AddOrdersResponse
    val Response: AddOrdersResponse.type = AddOrdersResponse
  final case class AddOrdersResponse(eventId: EventId) extends ControllerCommand.Response
  object AddOrdersResponse:
    implicit val jsonCodec: Codec.AsObject[AddOrdersResponse] = deriveCodec

  final case class CancelOrders(
    orderIds: immutable.Iterable[OrderId],
    mode: CancellationMode = CancellationMode.FreshOrStarted())
  extends ControllerCommand, Big:
    type Response = Response.Accepted
    override def toShortString = s"CancelOrders(${orderIds.size} orders, ${orderIds.take(3).map(o => o.toString + ", ").mkString} ...)"
  object CancelOrders:
    implicit val jsonEncoder: Encoder.AsObject[CancelOrders] = o =>
      JsonObject.fromIterable(
        ("orderIds" -> o.orderIds.asJson) ::
          (o.mode != CancellationMode.Default).thenList("mode" -> o.mode.asJson))

    implicit val jsonDecoder: Decoder[CancelOrders] = c =>
      for
        orderIds <- c.get[Vector[OrderId]]("orderIds")
        mode <- c.getOrElse[CancellationMode]("mode")(CancellationMode.Default)
      yield CancelOrders(orderIds, mode)

  final case class PostNotice(noticeId: NoticeId, endOfLife: Option[Timestamp] = None)
  extends ControllerCommand:
    type Response = Response.Accepted

  object PostNotice:
    private val codec = deriveConfiguredCodec[PostNotice]
    given Encoder.AsObject[PostNotice] = codec

    given Decoder[PostNotice] = c =>
      if c.get[Json]("boardPath").isRight then
        // COMPATIBLE with v2.7.3
        case class PostNotice273(
          boardPath: BoardPath,
          private val noticeId: NoticeKey,
          endOfLife: Option[Timestamp] = None):
          def noticeKey = noticeId
        deriveDecoder[PostNotice273].apply(c).map: o =>
          PostNotice(PlanId.Global / o.boardPath / o.noticeKey, o.endOfLife)
      else
        codec(c)

  final case class DeleteNotice(noticeId: NoticeId)
  extends ControllerCommand:
    type Response = Response.Accepted

  object DeleteNotice:
    private val codec = deriveConfiguredCodec[DeleteNotice]

    given Encoder.AsObject[DeleteNotice] = codec

    given Decoder[DeleteNotice] = c =>
      if c.get[Json]("boardPath").isRight then
        // COMPATIBLE with v2.7.3
        case class DeleteNotice273(boardPath: BoardPath, private val noticeId: NoticeKey):
          def noticeKey = noticeId
        deriveDecoder[DeleteNotice273].apply(c).map: o =>
          DeleteNotice(PlanId.Global / o.boardPath / o.noticeKey)
      else
        codec(c)


  final case class ChangeGlobalToPlannableBoard(
    plannableBoard: PlannableBoard,
    planSchemaId: PlanSchemaId,
    splitNoticeKey: ExprFunction)
  extends ControllerCommand:

    def evalSplitNoticeKey(noticeKey: NoticeKey): Checked[(PlanKey, NoticeKey)] =
      given Scope = Scope.empty
      for
        result <- splitNoticeKey.eval(StringValue(noticeKey.string))
        pair <- result.asPair[StringValue, StringValue]
        planKey <- PlanKey.checked(pair._1.string)
        noticeKey <- NoticeKey.checked(pair._2.string)
      yield
        (planKey, noticeKey)


  final case class ChangePlannableToGlobalBoard(
    globalBoard: GlobalBoard,
    planSchemaId: PlanSchemaId,
    makeNoticeKey: ExprFunction)
  extends ControllerCommand:

    def evalMakeNoticeKey(planKey: PlanKey, noticeKey: NoticeKey): Checked[NoticeKey] =
      given Scope = Scope.empty
      makeNoticeKey.eval(StringValue(planKey.string), StringValue(noticeKey.string))
        .flatMap(_.asString).flatMap(NoticeKey.checked)


  final case class DeleteOrdersWhenTerminated(orderIds: immutable.Iterable[OrderId])
  extends ControllerCommand, Big:
    type Response = Response.Accepted
    override def toShortString = s"DeleteOrdersWhenTerminated(${orderIds.size} orders, ${
      orderIds.take(3).map(o => o.toString + ", ").mkString} ...)"

  final case class AnswerOrderPrompt(orderId: OrderId)
  extends ControllerCommand:
    type response = Response.Accepted

  final case class NoOperation(duration: Option[FiniteDuration] = None)
  extends ControllerCommand:
    type Response = Response.Accepted

  type EmitTestEvent = EmitTestEvent.type
  /** For tests only. */
  case object EmitTestEvent extends ControllerCommand:
    type Response = Response.Accepted

  /** Controller stops immediately with exit(). */
  final case class EmergencyStop(restart: Boolean = false) extends ControllerCommand:
    type Response = Response.Accepted
  object EmergencyStop:
    implicit val jsonEncoder: Encoder.AsObject[EmergencyStop] = o =>
      JsonObject.fromIterable(
        o.restart.thenList("restart" -> Json.True))

    implicit val jsonDecoder: Decoder[EmergencyStop] = c =>
      for
        restart <- c.getOrElse[Boolean]("restart")(false)
      yield EmergencyStop(restart)

  /** Some outer component no longer needs the events until (including) the given `untilEventId`.
    * JS7 may delete these events to reduce the journal,
    * keeping all events after `untilEventId`.
    */
  final case class ReleaseEvents(untilEventId: EventId) extends ControllerCommand:
    type Response = Response.Accepted

  /** Shut down the Controller properly. */
  final case class ShutDown(
    restart: Boolean = false,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false,
    /*for test only*/dontNotifyActiveNode: Boolean = false)
  extends ControllerCommand:
    type Response = Response.Accepted

    def isFailOrSwitchover: Boolean =
      isFailover || isSwitchover

    def isFailover: Boolean =
      clusterAction contains ShutDown.ClusterAction.Failover

    private def isSwitchover: Boolean =
      clusterAction contains ShutDown.ClusterAction.Switchover

  object ShutDown:
    sealed trait ClusterAction
    object ClusterAction:
      case object Switchover extends ClusterAction
      case object Failover extends ClusterAction
      implicit val jsonCodec: TypedJsonCodec[ClusterAction] = TypedJsonCodec[ClusterAction](
        Subtype(Switchover),
        Subtype(Failover))

    implicit val jsonEncoder: Encoder.AsObject[ShutDown] = o =>
      JsonObject(
        "restart" -> o.restart.?.asJson,
        "clusterAction" -> o.clusterAction.asJson,
        "suppressSnapshot" -> o.suppressSnapshot.?.asJson,
        "dontNotifyActiveNode" -> o.dontNotifyActiveNode.?.asJson)

    implicit val jsonDecoder: Decoder[ShutDown] = c =>
      for
        restart <- c.getOrElse[Boolean]("restart")(false)
        clusterAction <- c.get[Option[ClusterAction]]("clusterAction")
        suppressSnapshot <- c.getOrElse[Boolean]("suppressSnapshot")(false)
        dontNotifyActiveNode <- c.getOrElse[Boolean]("dontNotifyActiveNode")(false)
      yield ShutDown(restart, clusterAction, suppressSnapshot, dontNotifyActiveNode)

  /** Go, Order! */
  final case class GoOrder(orderId: OrderId, position: Position)
  extends ControllerCommand:
    type Response = Response.Accepted

  final case class ResumeOrder(
    orderId: OrderId,
    position: Option[Position] = None,
    historyOperations: Seq[OrderResumed.HistoryOperation] = Nil,
    asSucceeded: Boolean = false,
    restartKilledJob: Option[Boolean] = None)
  extends ControllerCommand, Big:
    type Response = Response.Accepted

  final case class ResumeOrders(
    orderIds: immutable.Iterable[OrderId],
    asSucceeded: Boolean = false,
    restartKilledJob: Option[Boolean] = None)
  extends ControllerCommand, Big:
    type Response = Response.Accepted

  final case class SuspendOrders(
    orderIds: immutable.Iterable[OrderId],
    mode: SuspensionMode = SuspensionMode.standard)
  extends ControllerCommand, Big:
    type Response = Response.Accepted

  final case class ChangeOrder(
    orderId: OrderId,
    priority: Option[BigDecimal])
  extends ControllerCommand, Big:
    type Response = Response.Accepted

  /** Transfer all Orders from the given Workflow to the newest version.
   * @param workflowId denotes the Workflow from which all Orders are transferred
   */
  final case class TransferOrders(workflowId: WorkflowId)
  extends ControllerCommand:
    type Response = Response.Accepted

  final case class ChangePlanSchema(
    planSchemaId: PlanSchemaId,
    namedValues: Option[NamedValues] = None,
    finishedPlanRetentionPeriod: Option[FiniteDuration] = None)
  extends ControllerCommand:
    type Response = Response.Accepted

  final case class ChangePlan(planId: PlanId, status: PlanStatus)
  extends ControllerCommand:
    type Response = Response.Accepted

  /** Command to control all Workflows (all versions) of a WorkflowPath. */
  final case class ControlWorkflowPath(
    workflowPath: WorkflowPath,
    suspend: Option[Boolean] = None,
    skip: Map[Label, Boolean] = Map.empty)
  extends ControllerCommand:
    type Response = Response.Accepted

  /** Command to control a Workflow (a specific version). */
  final case class ControlWorkflow(
    workflowId: WorkflowId,
    addBreakpoints: Set[Position] = Set.empty,
    removeBreakpoints: Set[Position] = Set.empty)
  extends ControllerCommand:
    type Response = Response.Accepted

  case object TakeSnapshot extends ControllerCommand:
    type Response = Response.Accepted

  final case class ClusterAppointNodes(
    idToUri: Map[NodeId, Uri],
    activeId: NodeId)
  extends ControllerCommand:
    type Response = Response.Accepted

  final case class ClusterSwitchOver(agentPath: Option[AgentPath] = None)
  extends ControllerCommand:
    type Response = Response.Accepted

  final case class ConfirmClusterNodeLoss(
    agentPath: AgentPath,
    lostNodeId: NodeId,
    confirmer: String)
  extends ControllerCommand:
    type Response = Response.Accepted

  final case class ResetAgent(agentPath: AgentPath, force: Boolean = false)
  extends ControllerCommand:
    type Response = Response.Accepted

  final case class ResetSubagent(subagentId: SubagentId, force: Boolean = false)
  extends ControllerCommand:
    type Response = Response.Accepted

  sealed trait Response

  object Response:
    type Accepted = Accepted.type
    case object Accepted extends Response

    implicit val ResponseJsonCodec: TypedJsonCodec[Response] = TypedJsonCodec[Response](
      Subtype(Accepted),
      Subtype.named(deriveCodec[AddOrder.Response], "AddOrder.Response"),
      Subtype.named1[AddOrders.Response]("AddOrders.Response"),
      Subtype.named(deriveCodec[Batch.Response], "BatchResponse"))

  implicit val jsonCodec: TypedJsonCodec[ControllerCommand] = TypedJsonCodec[ControllerCommand](
    Subtype(deriveConfiguredCodec[Batch]),
    Subtype(deriveConfiguredCodec[AddOrder]),
    Subtype(deriveConfiguredCodec[AddOrders]),
    Subtype[CancelOrders],
    Subtype[PostNotice],
    Subtype[DeleteNotice],
    Subtype(deriveCodecWithDefaults[ChangeGlobalToPlannableBoard]),
    Subtype(deriveCodecWithDefaults[ChangePlannableToGlobalBoard]),
    Subtype(deriveConfiguredCodec[DeleteOrdersWhenTerminated]),
    Subtype(deriveConfiguredCodec[AnswerOrderPrompt]),
    Subtype(deriveConfiguredCodec[NoOperation]),
    Subtype(EmitTestEvent),
    Subtype[EmergencyStop],
    Subtype(deriveConfiguredCodec[ReleaseEvents]),
    Subtype[ShutDown],
    Subtype(deriveConfiguredCodec[GoOrder]),
    Subtype(deriveConfiguredCodec[ResumeOrder]),
    Subtype(deriveConfiguredCodec[ResumeOrders]),
    Subtype(deriveConfiguredCodec[SuspendOrders]),
    Subtype(deriveConfiguredCodec[ChangeOrder]),
    Subtype(deriveConfiguredCodec[TransferOrders]),
    Subtype(deriveConfiguredCodec[ChangePlanSchema]),
    Subtype(deriveConfiguredCodec[ChangePlan]),
    Subtype(deriveConfiguredCodec[ControlWorkflowPath]),
    Subtype(deriveConfiguredCodec[ControlWorkflow]),
    Subtype(deriveConfiguredCodec[ClusterAppointNodes]),
    Subtype(deriveConfiguredCodec[ClusterSwitchOver]),
    Subtype(deriveConfiguredCodec[ConfirmClusterNodeLoss]),
    Subtype(deriveConfiguredCodec[ResetAgent]),
    Subtype(deriveConfiguredCodec[ResetSubagent]),
    Subtype(TakeSnapshot))

  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder,
    checkedJsonEncoder[Int], checkedJsonDecoder[Int],
    versionedItemPathJsonCodec))
