package com.sos.jobscheduler.agent.data.views

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.data.workflow.JobPath
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TaskOverview(
  jobPath: JobPath,
  taskId: AgentTaskId,
  pid: Option[Pid] = None,
  startedAt: Instant)

object TaskOverview {
  private implicit def InstantJsonFormat = JavaTimeJsonFormats.NumericInstantJsonFormat  // Override default

  implicit val MyJsonFormat = jsonFormat4(apply)
}
