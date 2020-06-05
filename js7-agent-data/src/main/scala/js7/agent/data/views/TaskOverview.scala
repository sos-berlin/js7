package js7.agent.data.views

import io.circe.generic.JsonCodec
import js7.agent.data.AgentTaskId
import js7.base.time.Timestamp
import js7.common.process.Processes.Pid
import js7.data.job.JobKey

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class TaskOverview(
  jobKey: JobKey,
  taskId: AgentTaskId,
  pid: Option[Pid] = None,
  startedAt: Timestamp)
