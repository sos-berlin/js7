package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.MasterCommand._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, ObjectEncoder}
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

    def toFreshOrder = FreshOrder(id, workflowPath, scheduledAt, payload)

    type MyResponse = Response.Accepted
  }
  object AddOrderIfNew {
    def fromFreshOrder(order: FreshOrder) =
      AddOrderIfNew(order.id, order.workflowPath, order.scheduledAt, order.payload)

    def fromOrder(order: Order[Order.Fresh]): AddOrderIfNew =
      fromFreshOrder(FreshOrder.fromOrder(order))

    private[MasterCommand] implicit val jsonCodec: ObjectEncoder[AddOrderIfNew] =
      _.toFreshOrder.asJsonObject

    private[MasterCommand] implicit val jsonDecoder: Decoder[AddOrderIfNew] =
      _.as[FreshOrder] map fromFreshOrder
  }

  case object EmergencyStop extends MasterCommand {
    type MyResponse = Response.Accepted
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
    Subtype(EmergencyStop),
    Subtype[AddOrderIfNew],
    Subtype(deriveCodec[ScheduleOrdersEvery]),
    Subtype(deriveCodec[ReadConfigurationDirectory]),
    Subtype(Terminate))
}
