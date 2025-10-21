package js7.data.order

import cats.instances.option.*
import cats.syntax.traverse.*
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.event.KeyedEvent
import js7.data.item.VersionId
import js7.data.order.OrderEvent.OrderAdded
import js7.data.orderwatch.ExternalOrderKey
import js7.data.plan.PlanId
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchPath, Position, PositionOrLabel}
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
final case class FreshOrder(
  id: OrderId,
  workflowPath: WorkflowPath,
  arguments: NamedValues = Map.empty,
  planId: PlanId = PlanId.Global,
  scheduledFor: Option[Timestamp] = None,
  priority: BigDecimal = Order.DefaultPriority,
  deleteWhenTerminated: Boolean = false,
  forceAdmission: Boolean = false,
  innerBlock: BranchPath = BranchPath.empty,
  startPosition: Option[PositionOrLabel] = None,
  stopPositions: Set[PositionOrLabel] = Set.empty)
extends
  MinimumOrder:

  workflowPath.requireNonAnonymous()

  def toOrderAdded(
    versionId: VersionId,
    preparedArguments: NamedValues,
    externalOrderKey: Option[ExternalOrderKey] = None,
    startPosition: Option[Position] = None)
  : KeyedEvent[OrderAdded] =
    id <-: OrderAdded(workflowPath ~ versionId, preparedArguments, planId, scheduledFor, priority,
      externalOrderKey,
      deleteWhenTerminated = deleteWhenTerminated,
      forceAdmission = forceAdmission,
      innerBlock, startPosition, stopPositions)

  override def toString =
    s"FreshOrder($id $workflowPath $planId${
      scheduledFor.fold("")(o => s" scheduledFor=$o")}${
      (priority != Order.DefaultPriority) ?? s" priority=$priority"}${
      deleteWhenTerminated ?? " deleteWhenTerminated"}${
      forceAdmission ?? " forceAdmission"}${
      innerBlock.ifNonEmpty.fold("")(o => s"innerBlock=$o")}${
      startPosition.fold("")(o => s" start=$o")}${
      stopPositions.ifNonEmpty.fold("")(o => s" stop=${o.mkString(" ")}")}${
      arguments.map((k, v) => s"$k=$v").mkString(" ")})"


object FreshOrder:
  def apply(
    id: OrderId,
    workflowPath: WorkflowPath,
    arguments: NamedValues = NamedValues.empty,
    planId: PlanId = PlanId.Global,
    scheduledFor: Option[Timestamp] = None,
    priority: BigDecimal = Order.DefaultPriority,
    deleteWhenTerminated: Boolean = false,
    forceAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[PositionOrLabel] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty[PositionOrLabel])
  : FreshOrder =
    checked(id, workflowPath, arguments, planId, scheduledFor, priority,
      deleteWhenTerminated, forceAdmission,
      innerBlock, startPosition, stopPositions
    ).orThrow

  @TestOnly
  def unchecked(
    id: OrderId,
    workflowPath: WorkflowPath,
    arguments: NamedValues = Map.empty,
    planId: PlanId = PlanId.Global,
    scheduledFor: Option[Timestamp] = None,
    priority: BigDecimal = Order.DefaultPriority,
    deleteWhenTerminated: Boolean = false,
    forceAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[PositionOrLabel] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  : FreshOrder =
    new FreshOrder(id, workflowPath, arguments, planId, scheduledFor, priority,
      deleteWhenTerminated, forceAdmission,
      innerBlock, startPosition, stopPositions)

  def checked(
    id: OrderId,
    workflowPath: WorkflowPath,
    arguments: NamedValues = Map.empty,
    planId: PlanId = PlanId.Global,
    scheduledFor: Option[Timestamp] = None,
    priority: BigDecimal = Order.DefaultPriority,
    deleteWhenTerminated: Boolean = false,
    forceAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[PositionOrLabel] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  : Checked[FreshOrder] =
    for
      checkedId <- id.checkedNameSyntax
      _ <- scheduledFor.traverse(_.checkFiniteDurationCompatible)
    yield
      new FreshOrder(checkedId, workflowPath, arguments, planId, scheduledFor, priority,
        deleteWhenTerminated, forceAdmission,
        innerBlock, startPosition, stopPositions)

  implicit val jsonEncoder: Encoder.AsObject[FreshOrder] =
    o => JsonObject(
      "id" -> o.id.asJson,
      "workflowPath" -> o.workflowPath.asJson,
      "arguments" -> o.arguments.??.asJson,
      "planId" -> o.planId.asJson,
      "scheduledFor" -> o.scheduledFor.asJson,
      "priority" -> ((o.priority != Order.DefaultPriority) ? o.priority).asJson,
      "deleteWhenTerminated" -> o.deleteWhenTerminated.?.asJson,
      "forceAdmission" -> o.forceAdmission.?.asJson,
      "innerBlock" -> (o.innerBlock.nonEmpty ? o.innerBlock).asJson,
      "startPosition" -> o.startPosition.asJson,
      "stopPositions" -> (o.stopPositions.nonEmpty ? o.stopPositions).asJson)

  implicit val jsonDecoder: Decoder[FreshOrder] =
    c => for
      id <- c.get[OrderId]("id")
      workflowPath <- c.get[WorkflowPath]("workflowPath")
      planId <- c.get[PlanId]("planId")
      arguments <- c.getOrElse[NamedValues]("arguments")(NamedValues.empty)
      scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
      priority <- c.getOrElse[BigDecimal]("priority")(Order.DefaultPriority)
      deleteWhenTerminated <- c.getOrElse[Boolean]("deleteWhenTerminated")(false)
      forceAdmission <- c.get[Boolean]("forceJobAdmission"/*COMPATIBLE with 2.8.1*/).orElse:
        c.getOrElse[Boolean]("forceAdmission")(false)
      innerBlock <- c.getOrElse[BranchPath]("innerBlock")(BranchPath.empty)
      startPosition <- c.get[Option[PositionOrLabel]]("startPosition")
      stopPositions <- c.getOrElse[Set[PositionOrLabel]]("stopPositions")(Set.empty)
      order <-
        checked(id, workflowPath, arguments, planId, scheduledFor, priority,
          deleteWhenTerminated, forceAdmission,
          innerBlock, startPosition, stopPositions
        ).toDecoderResult(c.history)
    yield order
