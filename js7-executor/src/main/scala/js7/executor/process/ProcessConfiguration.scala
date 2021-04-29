package js7.executor.process

import java.nio.file.Path
import js7.base.io.process.Processes.Pid
import js7.data.job.TaskId
import js7.executor.configuration.ProcessKillScript

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  workingDirectory: Option[Path] = None,
  additionalEnvironment: Map[String, String] = Map(),
  maybeTaskId: Option[TaskId] = None,
  killScriptOption: Option[ProcessKillScript] = None)
{
  require(killScriptOption.isEmpty || maybeTaskId.nonEmpty, "killScriptFile requires idString")

  for (id <- maybeTaskId) require(id.nonEmpty)

  def idArgumentOption = maybeTaskId.map(o => s"--agent-task-id=${o.string}")

  def toKillScriptCommandArgumentsOption(pid: Option[Pid]) =
    for {
      id <- maybeTaskId
      killScript <- killScriptOption
    } yield killScript.toCommandArguments(id, pid)
}

object ProcessConfiguration
{
  def forTest = ProcessConfiguration()
}
