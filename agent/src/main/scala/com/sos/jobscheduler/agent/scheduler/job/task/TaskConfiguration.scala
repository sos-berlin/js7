package js7.agent.scheduler.job.task

import js7.agent.scheduler.job.ShellReturnValuesProvider
import js7.data.job.JobKey
import js7.data.workflow.instructions.executable.WorkflowJob
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class TaskConfiguration(
  jobKey: JobKey,
  workflowJob: WorkflowJob,
  shellFile: Path,
  shellReturnValuesProvider: ShellReturnValuesProvider)
