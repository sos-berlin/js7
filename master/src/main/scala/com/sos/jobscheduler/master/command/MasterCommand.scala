package com.sos.jobscheduler.master.command

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.command.MasterCommand._
import java.time.Duration
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterCommand {
  type MyResponse <: Response
}

object MasterCommand {
  final case class AddOrderIfNew(order: Order[Order.Idle]) extends MasterCommand {
    type MyResponse = Response.Accepted.type
  }

  final case class ScheduleOrdersEvery(every: Duration) extends MasterCommand

  case object Terminate extends MasterCommand {
    type MyResponse = Response.Accepted.type
  }

  sealed trait Response

  object Response {
    case object Accepted extends Response

    implicit val ResponseJsonFormat = TypedJsonFormat[Response](
      Subtype(jsonFormat0(() ⇒ Response.Accepted))
    )
  }

  implicit val jsonFormat = TypedJsonFormat[MasterCommand](
    Subtype(jsonFormat1(AddOrderIfNew)),
    Subtype(jsonFormat1(ScheduleOrdersEvery)),
    Subtype(jsonFormat0(() ⇒ Terminate)))
}
