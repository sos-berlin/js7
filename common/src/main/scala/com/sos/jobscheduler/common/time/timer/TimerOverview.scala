package com.sos.jobscheduler.common.time.timer

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class TimerOverview(at: Instant, name: String)

object TimerOverview {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
