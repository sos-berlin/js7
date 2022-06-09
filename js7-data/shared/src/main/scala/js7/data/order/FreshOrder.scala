package js7.data.order

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.typeclasses.IsEmpty.syntax._
import js7.data.event.KeyedEvent
import js7.data.item.VersionId
import js7.data.order.OrderEvent.OrderAdded
import js7.data.orderwatch.ExternalOrderKey
import js7.data.value.NamedValues
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
final case class FreshOrder private(
  id: OrderId,
  workflowPath: WorkflowPath,
  arguments: NamedValues = Map.empty,
  scheduledFor: Option[Timestamp] = None,
  deleteWhenTerminated: Boolean = false,
  startPosition: Option[Position] = None,
  stopPosition: Option[Position] = None)
{
  workflowPath.requireNonAnonymous()

  def toOrderAdded(
    versionId: VersionId,
    preparedArguments: NamedValues,
    externalOrderKey: Option[ExternalOrderKey] = None)
  : KeyedEvent[OrderAdded] =
    id <-: OrderAdded(workflowPath ~ versionId, preparedArguments, scheduledFor, externalOrderKey,
      deleteWhenTerminated = deleteWhenTerminated,
      startPosition, stopPosition)
}

object FreshOrder
{
  def apply(
    id: OrderId,
    workflowPath: WorkflowPath,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    deleteWhenTerminated: Boolean = false,
    startPosition: Option[Position] = None,
    stopPosition: Option[Position] = None)
  : FreshOrder =
    checked(id, workflowPath, arguments, scheduledFor, deleteWhenTerminated,
      startPosition, stopPosition)
      .orThrow

  @TestOnly
  def unchecked(
    id: OrderId,
    workflowPath: WorkflowPath,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    deleteWhenTerminated: Boolean = false,
    startPosition: Option[Position] = None,
    stopPosition: Option[Position] = None)
  : FreshOrder =
    new FreshOrder(id, workflowPath, arguments, scheduledFor, deleteWhenTerminated,
      startPosition, stopPosition)

  def checked(
    id: OrderId,
    workflowPath: WorkflowPath,
    arguments: NamedValues = Map.empty,
    scheduledFor: Option[Timestamp] = None,
    deleteWhenTerminated: Boolean = false,
    startPosition: Option[Position] = None,
    stopPosition: Option[Position] = None)
  : Checked[FreshOrder] =
    for (checkedId <- id.checkedNameSyntax)
      yield new FreshOrder(checkedId, workflowPath, arguments, scheduledFor, deleteWhenTerminated,
        startPosition, stopPosition)

  def fromOrder(order: Order[Order.Fresh]): FreshOrder =
    new FreshOrder(order.id, order.workflowPath, order.arguments, order.scheduledFor,
      order.deleteWhenTerminated)

  implicit val jsonEncoder: Encoder.AsObject[FreshOrder] =
    o => JsonObject(
      "id" -> o.id.asJson,
      "workflowPath" -> o.workflowPath.asJson,
      "scheduledFor" -> o.scheduledFor.asJson,
      "arguments" -> o.arguments.??.asJson,
      "deleteWhenTerminated" -> o.deleteWhenTerminated.?.asJson,
      "startPosition" -> o.startPosition.asJson,
      "stopPosition" -> o.stopPosition.asJson)

  implicit val jsonDecoder: Decoder[FreshOrder] =
    c => for {
      id <- c.get[OrderId]("id")
      workflowPath <- c.get[WorkflowPath]("workflowPath")
      scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
      arguments <- c.getOrElse[NamedValues]("arguments")(NamedValues.empty)
      deleteWhenTerminated <- c.getOrElse[Boolean]("deleteWhenTerminated")(false)
      startPosition <- c.get[Option[Position]]("startPosition")
      stopPosition <- c.get[Option[Position]]("stopPosition")
      order <- checked(id, workflowPath, arguments, scheduledFor, deleteWhenTerminated,
        startPosition, stopPosition)
        .toDecoderResult(c.history)
    } yield order
}
