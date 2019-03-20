package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class FreshOrder(
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
  def fromOrder(order: Order[Order.Fresh]): FreshOrder =
    new FreshOrder(order.id, order.workflowId.path, order.state.scheduledFor, order.arguments)

  implicit val jsonCodec: ObjectEncoder[FreshOrder] =
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
      arguments <- c.get[Option[Map[String, String]]]("arguments") map (_ getOrElse Map.empty)
    } yield FreshOrder(id, workflowPath, scheduledFor, arguments)
}
