package com.sos.jobscheduler.agent.data.views

import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskRegisterOverview(
  currentTaskCount: Int,
  totalTaskCount: Int)

object TaskRegisterOverview {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
