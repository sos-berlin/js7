package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderScheduleEndedAt(instant: Instant)

object OrderScheduleEndedAt {
  implicit val jsonFormat = jsonFormat1(apply)
}
