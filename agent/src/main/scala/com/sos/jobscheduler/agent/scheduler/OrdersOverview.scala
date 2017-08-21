package com.sos.jobscheduler.agent.scheduler

import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrdersOverview(
  currentTaskCount: Int,
  totalTaskCount: Int)

object OrdersOverview {
  implicit val jsonFormat = jsonFormat2(apply)
}
