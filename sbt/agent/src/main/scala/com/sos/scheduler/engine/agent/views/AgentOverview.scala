package com.sos.scheduler.engine.agent.views

import com.google.inject.ProvidedBy
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.system.SystemInformation
import com.sos.scheduler.engine.data.system.JavaInformation
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
