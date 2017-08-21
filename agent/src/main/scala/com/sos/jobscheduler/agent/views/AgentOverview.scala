package com.sos.jobscheduler.agent.views

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.data.system.JavaInformation
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class AgentOverview(
  version: String,
  startedAt: Instant,
  isTerminating: Boolean,
  system: SystemInformation,
  java: JavaInformation)

object AgentOverview {
  implicit val MyJsonFormat = jsonFormat5(apply)
}
