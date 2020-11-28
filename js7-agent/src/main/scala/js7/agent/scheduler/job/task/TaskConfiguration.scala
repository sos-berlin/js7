package js7.agent.scheduler.job.task

import js7.data.job.{CommandLine, JobKey}
import js7.data.workflow.instructions.executable.WorkflowJob

/**
  * @author Joacim Zschimmer
  */
final case class TaskConfiguration(
  jobKey: JobKey,
  workflowJob: WorkflowJob,
  commandLine: CommandLine)
