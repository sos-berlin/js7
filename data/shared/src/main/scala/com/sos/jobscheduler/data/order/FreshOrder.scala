package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath, WorkflowPosition}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class FreshOrder(
  id: OrderId,
  workflowPath: WorkflowPath,
  scheduledAt: Option[Timestamp] = None,
  payload: Payload = Payload.empty)
{
  workflowPath.requireNonAnonymous()

  def toOrder(versionId: VersionId): Order[Order.NotStarted] = {
    val firstPosition = Position(0)
    val state = scheduledAt match {
      case None ⇒ Order.StartNow
      case Some(ts) ⇒ Order.Scheduled(ts)
    }
    Order(id, WorkflowPosition(workflowPath % versionId, firstPosition), state, payload = payload)
  }
}

object FreshOrder
{
  def fromOrder(order: Order[Order.NotStarted]): FreshOrder =  {
    val scheduledAt = order.state match {
      case Order.StartNow ⇒ None
      case Order.Scheduled(ts) ⇒ Some(ts)
    }
    new FreshOrder(order.id, order.workflowId.path, scheduledAt, order.payload)
  }

  implicit val jsonCodec: ObjectEncoder[FreshOrder] =
    o ⇒ JsonObject(
      "id" → o.id.asJson,
      "workflowPath" → o.workflowPath.asJson,
      "scheduledAt" → o.scheduledAt.asJson,
      "variables" → ((o.payload != Payload.empty) ? o.payload.variables).asJson)

  implicit val jsonDecoder: Decoder[FreshOrder] =
    c ⇒ for {
      id ← c.get[OrderId]("id")
      workflowPath ← c.get[WorkflowPath]("workflowPath")
      scheduledAt ← c.get[Option[Timestamp]]("scheduledAt")
      payload ← c.get[Option[Map[String, String]]]("variables") map (_ map Payload.apply getOrElse Payload.empty)
    } yield FreshOrder(id, workflowPath, scheduledAt, payload)
}
