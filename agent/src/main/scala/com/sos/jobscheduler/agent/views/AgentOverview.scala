package com.sos.jobscheduler.agent.views

import com.google.inject.ProvidedBy
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.data.system.JavaInformation
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
@ProvidedBy(classOf[AgentOverviewProvider])
final case class AgentOverview(
  version: String,
  startedAt: Instant,
  currentTaskCount: Int,
  totalTaskCount: Int,
  isTerminating: Boolean,
  system: SystemInformation,
  java: JavaInformation)

object AgentOverview {

  implicit val MyJsonFormat = jsonFormat7(apply)
}
