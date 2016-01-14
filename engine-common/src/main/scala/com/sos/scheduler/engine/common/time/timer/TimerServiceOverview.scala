package com.sos.scheduler.engine.common.time.timer

import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class TimerServiceOverview(
  count: Int,
  completeCount: Int,
  wakeCount: Int,
  prematureWakeCount: Option[Int] = None,
  first: Option[TimerOverview] = None,
  last: Option[TimerOverview] = None
)

object TimerServiceOverview {
  implicit val MyJsonFormat = jsonFormat6(apply)
}
