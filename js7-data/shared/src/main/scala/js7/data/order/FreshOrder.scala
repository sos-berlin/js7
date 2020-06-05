package js7.data.order


import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.time.Timestamp
import js7.base.utils.ScalazStyle._
import js7.data.filebased.VersionId
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.{Position, WorkflowPosition}
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
final case class FreshOrder private(
  id: OrderId,
  workflowPath: WorkflowPath,
  scheduledFor: Option[Timestamp] = None,
  arguments: Map[String, String] = Map.empty)
{
  workflowPath.requireNonAnonymous()

  def toOrder(versionId: VersionId): Order[Order.Fresh] = {
    val firstPosition = Position(0)
    Order(id, WorkflowPosition(workflowPath ~ versionId, firstPosition), Order.Fresh(scheduledFor), arguments)
  }
}

object FreshOrder
{
  def apply(
    id: OrderId,
    workflowPath: WorkflowPath,
    scheduledFor: Option[Timestamp] = None,
    arguments: Map[String, String] = Map.empty)
  : FreshOrder =
    checked(id, workflowPath, scheduledFor, arguments).orThrow

  @TestOnly
  def unchecked(
    id: OrderId,
    workflowPath: WorkflowPath,
    scheduledFor: Option[Timestamp] = None,
    arguments: Map[String, String] = Map.empty)
  : FreshOrder =
    new FreshOrder(id, workflowPath, scheduledFor, arguments)

  def checked(
    id: OrderId,
    workflowPath: WorkflowPath,
    scheduledFor: Option[Timestamp] = None,
    arguments: Map[String, String] = Map.empty)
  : Checked[FreshOrder] =
    for (checkedId <- id.checkedNameSyntax)
      yield new FreshOrder(checkedId, workflowPath, scheduledFor, arguments)

  def fromOrder(order: Order[Order.Fresh]): FreshOrder =
    new FreshOrder(order.id, order.workflowId.path, order.state.scheduledFor, order.arguments)

  implicit val jsonCodec: Encoder.AsObject[FreshOrder] =
    o => JsonObject(
      "id" -> o.id.asJson,
      "workflowPath" -> o.workflowPath.asJson,
      "scheduledFor" -> o.scheduledFor.asJson,
      "arguments" -> (o.arguments.nonEmpty ? o.arguments).asJson)

  implicit val jsonDecoder: Decoder[FreshOrder] =
    c => for {
      id <- c.get[OrderId]("id")
      workflowPath <- c.get[WorkflowPath]("workflowPath")
      scheduledFor <- c.get[Option[Timestamp]]("scheduledFor")
      arguments <- c.get[Option[Map[String, String]]]("arguments").map(_ getOrElse Map.empty)
      order <- checked(id, workflowPath, scheduledFor, arguments).toDecoderResult(c.history)
    } yield order
}
