package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.order.OrderState
import spray.json.DefaultJsonProtocol._

final case class SinkNodeOverview(orderState: OrderState)
extends NodeOverview


object SinkNodeOverview {
  implicit val MyJsonFormat = jsonFormat1(apply)
}
