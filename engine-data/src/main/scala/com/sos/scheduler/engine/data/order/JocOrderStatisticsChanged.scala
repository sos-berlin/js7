package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.NoKeyEvent
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class JocOrderStatisticsChanged(orderStatistics: JocOrderStatistics) extends NoKeyEvent

object JocOrderStatisticsChanged {
  implicit val MyJsonFormat = jsonFormat1(apply)
}
