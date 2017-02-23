package com.sos.scheduler.engine.master.command

import com.sos.scheduler.engine.data.engine2.order.Order
import com.sos.scheduler.engine.master.command.MasterCommand._

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

  sealed trait Response

  object Response {
    case object Accepted extends Response
  }
}
