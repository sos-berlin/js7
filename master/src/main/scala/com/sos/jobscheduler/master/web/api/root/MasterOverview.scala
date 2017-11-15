package com.sos.jobscheduler.master.web.api.root

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.data.system.JavaInformation
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class MasterOverview(
  version: String,
  startedAt: Instant,
  orderCount: Int,
  system: SystemInformation,
  java: JavaInformation)

object MasterOverview {
  implicit val MyJsonFormat = jsonFormat5(apply)
}
