package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, ShellReturnValuesProvider}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class TaskConfiguration(
  jobConfiguration: JobConfiguration,
  shellFile: Path,
  shellReturnValuesProvider: ShellReturnValuesProvider)
{
  def jobPath = jobConfiguration.path
}
