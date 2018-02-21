package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.data.MasterCommand._
import io.circe.generic.JsonCodec
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterCommand {
  type MyResponse <: Response
}

object MasterCommand {
  intelliJuseImport(FiniteDurationJsonEncoder)

  @JsonCodec
  final case class AddOrderIfNew(order: Order[Order.NotStarted]) extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  @JsonCodec
  final case class ScheduleOrdersEvery(every: FiniteDuration) extends MasterCommand

  case object Terminate extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  case object ReadConfigurationDirectory extends MasterCommand {
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
    Subtype[ScheduleOrdersEvery],
    Subtype(ReadConfigurationDirectory),
    Subtype(Terminate))
}
