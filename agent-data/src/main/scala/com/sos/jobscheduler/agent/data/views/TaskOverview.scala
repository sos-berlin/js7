package com.sos.jobscheduler.agent.data.views

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.data.job.JobPath
import io.circe.generic.JsonCodec

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class TaskOverview(
  jobPath: JobPath,
  taskId: AgentTaskId,
  pid: Option[Pid] = None,
  startedAt: Timestamp)
