package js7.agent.scheduler.job.task

import js7.data.job.{CommandLine, JobKey, ReturnCode}
import js7.data.order.Outcome
import js7.data.value.NamedValues

/**
  * @author Joacim Zschimmer
  */
final case class TaskConfiguration(
  jobKey: JobKey,
  toOutcome: (NamedValues, ReturnCode) => Outcome.Completed,
  commandLine: CommandLine,
  v1Compatible: Boolean = false)
