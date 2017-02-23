package com.sos.jobscheduler.http.server.heartbeat

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.http.client.heartbeat.HeartbeatId
import com.sos.jobscheduler.http.server.heartbeat.HeartbeatView._
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class HeartbeatView(
  startCount: Int,
  count: Int,
  concurrentMaximum: Int,
  pendingOperations: Map[HeartbeatId, PendingOperation])

object HeartbeatView {
  final case class PendingOperation(
    startedAt: Instant,
    lastAt: Option[Instant],
    count: Int
  )

  implicit val pendingOperationJsonFormat = jsonFormat3(PendingOperation.apply)
  implicit val MyJsonFormat = jsonFormat4(apply)
}
