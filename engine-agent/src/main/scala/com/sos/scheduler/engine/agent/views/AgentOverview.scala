package com.sos.scheduler.engine.agent.views

import com.google.inject.ProvidedBy
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
@ProvidedBy(classOf[AgentOverviewProvider])
final case class AgentOverview(
  version: String,
  startedAt: Instant,
  processCount: Int)

object AgentOverview {
  implicit val MyJsonFormat = jsonFormat3(apply)
}
