package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, ShellReturnValuesProvider}
import com.sos.jobscheduler.data.job.JobPath
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class TaskConfiguration(
  jobConfiguration: JobConfiguration,
  shellFile: Path,
  shellReturnValuesProvider: ShellReturnValuesProvider)
{
  def jobPath: JobPath = jobConfiguration.path
}
