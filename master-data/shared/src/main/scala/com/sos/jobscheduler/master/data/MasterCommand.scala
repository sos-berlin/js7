package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath, WorkflowPosition}
import com.sos.jobscheduler.master.data.MasterCommand._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterCommand {
  type MyResponse <: Response
}

object MasterCommand {
  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder))

  final case class AddOrderIfNew(
    id: OrderId,
    workflowPath: WorkflowPath,
    scheduledAt: Option[Timestamp],
    payload: Payload = Payload.empty)
  extends MasterCommand {
    workflowPath.requireNonAnonymous()

    def toOrder(versionId: VersionId): Order[Order.NotStarted] = {
      val firstPosition = Position(0)
      val state = scheduledAt match {
        case None ⇒ Order.StartNow
        case Some(ts) ⇒ Order.Scheduled(ts)
      }
      Order(id, WorkflowPosition(workflowPath % versionId, firstPosition), state, payload = payload)
    }

    type MyResponse = Response.Accepted
  }
  object AddOrderIfNew {
    def fromOrder(order: Order[Order.NotStarted]): AddOrderIfNew =  {
      val scheduledAt = order.state match {
        case Order.StartNow ⇒ None
        case Order.Scheduled(ts) ⇒ Some(ts)
      }
      new AddOrderIfNew(order.id, order.workflowId.path, scheduledAt, order.payload)
    }

    private[MasterCommand] implicit val jsonCodec: ObjectEncoder[AddOrderIfNew] =
      o ⇒ JsonObject(
        "id" → o.id.asJson,
        "workflowPath" → o.workflowPath.asJson,
        "scheduledAt" → o.scheduledAt.asJson,
        "variables" → ((o.payload != Payload.empty) ? o.payload.variables).asJson)

    private[MasterCommand] implicit val jsonDecoder: Decoder[AddOrderIfNew] =
      c ⇒ for {
        id ← c.get[OrderId]("id")
        workflowPath ← c.get[WorkflowPath]("workflowPath")
        scheduledAt ← c.get[Option[Timestamp]]("scheduledAt")
        payload ← c.get[Option[Map[String, String]]]("variables") map (_ map Payload.apply getOrElse Payload.empty)
      } yield AddOrderIfNew(id, workflowPath, scheduledAt, payload)
  }

  final case class ScheduleOrdersEvery(every: FiniteDuration) extends MasterCommand

  case object Terminate extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  final case class ReadConfigurationDirectory(versionId: VersionId) extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  sealed trait Response

  object Response {
    sealed trait Accepted extends Response
    case object Accepted extends Accepted

    implicit val ResponseJsonCodec = TypedJsonCodec[Response](
      Subtype(Accepted))
  }

  implicit val jsonCodec = TypedJsonCodec[MasterCommand](
    Subtype[AddOrderIfNew],
    Subtype(deriveCodec[ScheduleOrdersEvery]),
    Subtype(deriveCodec[ReadConfigurationDirectory]),
    Subtype(Terminate))
}
