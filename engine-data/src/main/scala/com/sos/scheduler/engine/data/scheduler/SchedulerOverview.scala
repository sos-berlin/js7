package com.sos.scheduler.engine.data.scheduler

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.system.SystemInformation
import com.sos.scheduler.engine.data.scheduler.SchedulerStates.SchedulerStateJsonFormat
import com.sos.scheduler.engine.data.system.JavaInformation
import java.time.Instant
import spray.json.DefaultJsonProtocol._

final case class SchedulerOverview(
  version: String,
  startedAt: Instant,
  schedulerId: SchedulerId,
  httpPort: Option[Int],
  udpPort: Option[Int],
  pid: Int,
  state: SchedulerState,
  system: SystemInformation,
  java: JavaInformation)

object SchedulerOverview {
  implicit val MyJsonFormat = jsonFormat9(apply)
}
