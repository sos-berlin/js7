package com.sos.scheduler.engine.agent.data.views

import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskHandlerOverview(
  currentTaskCount: Int,
  totalTaskCount: Int)

object TaskHandlerOverview {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
