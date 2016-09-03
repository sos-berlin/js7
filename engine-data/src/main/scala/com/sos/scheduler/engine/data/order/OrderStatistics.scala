package com.sos.scheduler.engine.data.order

import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderStatistics(
  total: Int,
  notPlanned: Int,
  planned: Int,
  pending: Int,
  running: Int,
  inTask: Int,
  inProcess: Int,
  setback: Int,
  suspended: Int,
  blacklisted: Int,
  permanent: Int,
  fileOrder: Int)

object OrderStatistics {
  implicit val MyJsonFormat = jsonFormat12(apply)
}

// JOC2 dashboard: Pending, Running, Suspended, Setback, Waiting for Resource, Blacklist
