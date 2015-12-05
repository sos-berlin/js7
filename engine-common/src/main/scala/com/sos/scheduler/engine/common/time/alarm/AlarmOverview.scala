package com.sos.scheduler.engine.common.time.alarm

import java.time.Instant
import spray.json.DefaultJsonProtocol._
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._

/**
  * @author Joacim Zschimmer
  */
final case class AlarmOverview(at: Instant, name: String)

object AlarmOverview {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
