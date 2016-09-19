package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.NoKeyEvent
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderStatisticsChanged(orderStatistics: OrderStatistics) extends NoKeyEvent

object OrderStatisticsChanged {
  implicit val MyJsonFormat = jsonFormat1(apply)
}
