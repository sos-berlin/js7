package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.agent.scheduler.job.ShellReturnValuesProvider
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class TaskConfiguration(
  jobKey: JobKey,
  workflowJob: WorkflowJob,
  shellFile: Path,
  shellReturnValuesProvider: ShellReturnValuesProvider)
