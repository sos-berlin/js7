package js7.data.order

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
import js7.data.value.NamedValues
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
  scheduledFor: Option[Timestamp] = None,
  deleteWhenTerminated: Boolean = false,
  forceJobAdmission: Boolean = false,
  innerBlock: BranchPath = BranchPath.empty,
  startPosition: Option[PositionOrLabel] = None,
  stopPositions: Set[PositionOrLabel] = Set.empty):
  workflowPath.requireNonAnonymous()

  def toOrderAdded(
    versionId: VersionId,
    preparedArguments: NamedValues,
    externalOrderKey: Option[ExternalOrderKey] = None,
    startPosition: Option[Position] = None)
  : KeyedEvent[OrderAdded] =
    id <-: OrderAdded(workflowPath ~ versionId, preparedArguments, scheduledFor, externalOrderKey,
      deleteWhenTerminated = deleteWhenTerminated,
      forceJobAdmission = forceJobAdmission,
      innerBlock, startPosition, stopPositions)

object FreshOrder:
  def apply(
    id: OrderId,
    workflowPath: WorkflowPath,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    deleteWhenTerminated: Boolean = false,
    forceJobAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[PositionOrLabel] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  : FreshOrder =
    checked(id, workflowPath, arguments, scheduledFor,
      deleteWhenTerminated, forceJobAdmission,
      innerBlock, startPosition, stopPositions
    ).orThrow

  @TestOnly
  def unchecked(
    id: OrderId,
    workflowPath: WorkflowPath,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    deleteWhenTerminated: Boolean = false,
    forceJobAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[PositionOrLabel] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  : FreshOrder =
    new FreshOrder(id, workflowPath, arguments, scheduledFor,
      deleteWhenTerminated, forceJobAdmission,
      innerBlock, startPosition, stopPositions)

  def checked(
    id: OrderId,
    workflowPath: WorkflowPath,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    deleteWhenTerminated: Boolean = false,
    forceJobAdmission: Boolean = false,
    innerBlock: BranchPath = BranchPath.empty,
    startPosition: Option[PositionOrLabel] = None,
    stopPositions: Set[PositionOrLabel] = Set.empty)
  : Checked[FreshOrder] =
    for checkedId <- id.checkedNameSyntax
      yield new FreshOrder(checkedId, workflowPath, arguments, scheduledFor,
        deleteWhenTerminated, forceJobAdmission,
        innerBlock, startPosition, stopPositions)

  implicit val jsonEncoder: Encoder.AsObject[FreshOrder] =
    o => JsonObject(
      "id" -> o.id.asJson,
      "workflowPath" -> o.workflowPath.asJson,
      "scheduledFor" -> o.scheduledFor.asJson,
      "arguments" -> o.arguments.??.asJson,
      "deleteWhenTerminated" -> o.deleteWhenTerminated.?.asJson,
      "forceJobAdmission" -> o.forceJobAdmission.?.asJson,
      "innerBlock" -> (o.innerBlock.nonEmpty ? o.innerBlock).asJson,
      "startPosition" -> o.startPosition.asJson,
      "stopPositions" -> (o.stopPositions.nonEmpty ? o.stopPositions).asJson)

  implicit val jsonDecoder: Decoder[FreshOrder] =
    c => for
      id <- c.get[OrderId]("id")
      workflowPath <- c.get[WorkflowPath]("workflowPath")
      scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
      arguments <- c.getOrElse[NamedValues]("arguments")(NamedValues.empty)
      deleteWhenTerminated <- c.getOrElse[Boolean]("deleteWhenTerminated")(false)
      forceJobAdmission <- c.getOrElse[Boolean]("forceJobAdmission")(false)
      innerBlock <- c.getOrElse[BranchPath]("innerBlock")(BranchPath.empty)
      startPosition <- c.get[Option[PositionOrLabel]]("startPosition")
      stopPositions <- c.getOrElse[Set[PositionOrLabel]]("stopPositions")(Set.empty)
      order <-
        checked(id, workflowPath, arguments, scheduledFor,
          deleteWhenTerminated, forceJobAdmission,
          innerBlock, startPosition, stopPositions
        ).toDecoderResult(c.history)
    yield order
