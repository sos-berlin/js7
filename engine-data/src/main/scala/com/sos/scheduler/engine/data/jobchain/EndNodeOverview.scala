package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.order.OrderState
import spray.json.DefaultJsonProtocol._

final case class EndNodeOverview(orderState: OrderState)
extends NodeOverview


object EndNodeOverview {
  implicit val MyJsonFormat = jsonFormat1(apply)
}
