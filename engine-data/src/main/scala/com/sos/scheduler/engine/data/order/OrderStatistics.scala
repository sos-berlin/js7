package com.sos.scheduler.engine.data.order

import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderStatistics(
  plannedOrPending: Int,
  running: Int,
  suspended: Int,
  setback: Int,
  blacklisted: Int)

object OrderStatistics {
  implicit val MyJsonFormat = jsonFormat5(apply)
}

// JOC2 dashboard: Pending, Running, Suspended, Setback, Waiting for Resource, Blacklist
