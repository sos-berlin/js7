package com.sos.scheduler.engine.common.time.alarm

import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class AlarmClockOverview(
  count: Int,
  first: Option[AlarmOverview],
  last: Option[AlarmOverview]
)

object AlarmClockOverview {
  implicit val MyJsonFormat = jsonFormat3(apply)
}
