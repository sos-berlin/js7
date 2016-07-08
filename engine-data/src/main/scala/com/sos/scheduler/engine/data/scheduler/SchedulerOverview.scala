package com.sos.scheduler.engine.data.scheduler

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.data.scheduler.SchedulerStates.SchedulerStateJsonFormat
import java.time.Instant
import spray.json.DefaultJsonProtocol._

final case class SchedulerOverview(
  version: String,
  versionCommitHash: String,
  startInstant: Instant,
  instant: Instant,
  schedulerId: SchedulerId,
  tcpPort: Option[Int],
  udpPort: Option[Int],
  pid: Int,
  state: SchedulerState)

object SchedulerOverview {
  implicit val MyJsonFormat = jsonFormat9(apply)
}
