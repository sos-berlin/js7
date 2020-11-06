package js7.agent.scheduler.job.task

import java.nio.file.Path
import js7.data.job.JobKey
import js7.data.workflow.instructions.executable.WorkflowJob

/**
  * @author Joacim Zschimmer
  */
final case class TaskConfiguration(
  jobKey: JobKey,
  workflowJob: WorkflowJob,
  shellFile: Path)
