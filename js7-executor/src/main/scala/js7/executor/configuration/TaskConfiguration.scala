package js7.executor.configuration

import js7.base.io.process.{KeyLogin, ReturnCode}
import js7.data.job.{CommandLine, JobKey}
import js7.data.order.Outcome
import js7.data.value.NamedValues

/**
  * @author Joacim Zschimmer
  */
final case class TaskConfiguration(
  jobKey: JobKey,
  toOutcome: (NamedValues, ReturnCode) => Outcome.Completed,
  commandLine: CommandLine,
  login: Option[KeyLogin] = None,
  v1Compatible: Boolean = false)
