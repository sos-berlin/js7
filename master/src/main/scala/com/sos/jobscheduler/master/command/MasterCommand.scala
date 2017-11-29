package com.sos.jobscheduler.master.command

import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.command.MasterCommand._
import io.circe.generic.JsonCodec
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterCommand {
  type MyResponse <: Response
}

object MasterCommand {
  @JsonCodec
  final case class AddOrderIfNew(order: Order[Order.NotStarted]) extends MasterCommand {
    type MyResponse = Response.Accepted.type
  }

  @JsonCodec
  final case class ScheduleOrdersEvery(every: FiniteDuration) extends MasterCommand

  case object Terminate extends MasterCommand {
    type MyResponse = Response.Accepted.type
  }

  sealed trait Response

  object Response {
    case object Accepted extends Response

    implicit val ResponseJsonCodec = TypedJsonCodec[Response](
      Subtype(Accepted))
  }

  implicit val JsonCodec = TypedJsonCodec[MasterCommand](
    Subtype[AddOrderIfNew],
    Subtype[ScheduleOrdersEvery],
    Subtype(Terminate))
}
