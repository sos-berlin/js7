package com.sos.jobscheduler.agent.data.views

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.commands.StartTask
import com.sos.jobscheduler.base.sprayjson.InetAddressJsonSupport._
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits.InstantJsonFormat
import com.sos.jobscheduler.tunnel.data.TunnelId
import java.net.InetAddress
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskOverview(
  id: AgentTaskId,
  pid: Option[Long] = None,
  tunnelId: TunnelId,
  startedAt: Instant,
  startedByHttpIp: Option[InetAddress],
  startMeta: StartTask.Meta,
  arguments: Option[TaskOverview.Arguments])

object TaskOverview {
  implicit val ArgumentsJsonFormat = jsonFormat3(Arguments)
  implicit val MyJsonFormat = jsonFormat7(apply)

  final case class Arguments(
    language: String,
    javaClassName: Option[String],
    monitorCount: Int)
}
