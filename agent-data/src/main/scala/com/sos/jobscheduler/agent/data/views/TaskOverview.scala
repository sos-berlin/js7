package com.sos.jobscheduler.agent.data.views

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits.InstantJsonFormat
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.data.jobnet.JobPath
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskOverview(
  jobPath: JobPath,
  id: AgentTaskId,
  pid: Option[Pid] = None,
  startedAt: Instant)

object TaskOverview {
  implicit val MyJsonFormat = jsonFormat4(apply)
}
