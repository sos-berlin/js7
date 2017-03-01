package com.sos.jobscheduler.master.command

import com.sos.jobscheduler.data.engine2.order.Order
import com.sos.jobscheduler.master.command.MasterCommand._
import java.time.Duration

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

  sealed trait Response

  object Response {
    case object Accepted extends Response
  }
}
