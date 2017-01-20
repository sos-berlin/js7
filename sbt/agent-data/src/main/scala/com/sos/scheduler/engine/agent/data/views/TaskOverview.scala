package com.sos.scheduler.engine.agent.data.views

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commands.StartTask
import com.sos.scheduler.engine.base.sprayjson.InetAddressJsonSupport._
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits.InstantJsonFormat
import com.sos.scheduler.engine.tunnel.data.TunnelId
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
