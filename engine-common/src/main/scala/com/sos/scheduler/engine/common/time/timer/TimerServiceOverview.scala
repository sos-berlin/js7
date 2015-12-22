package com.sos.scheduler.engine.common.time.timer

import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class TimerServiceOverview(
  elapsedCount: Int,
  completeCount: Int,
  count: Int,
  first: Option[TimerOverview],
  last: Option[TimerOverview]
)

object TimerServiceOverview {
  implicit val MyJsonFormat = jsonFormat5(apply)
}
