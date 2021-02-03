package js7.taskserver.task.process

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Path
import js7.agent.data.ProcessKillScript
import js7.base.system.OperatingSystem.isWindows
import js7.common.process.Processes.Pid
import js7.data.job.TaskId
import js7.data.system.StdoutOrStderr

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  stdFileMap: Map[StdoutOrStderr, Path] = Map(),
  encoding: Charset,
  workingDirectory: Option[Path] = None,
  additionalEnvironment: Map[String, String] = Map(),
  maybeTaskId: Option[TaskId] = None,
  killScriptOption: Option[ProcessKillScript] = None)
{
  require(killScriptOption.isEmpty || maybeTaskId.nonEmpty, "killScriptFile requires idString")

  for (id <- maybeTaskId) require(id.nonEmpty)

  def files: Seq[Path] = stdFileMap.values.toList

  def idArgumentOption = maybeTaskId map { o => s"--agent-task-id=${o.string}" }

  def toKillScriptCommandArgumentsOption(pid: Option[Pid]) =
    for (id <- maybeTaskId; killScript <- killScriptOption) yield killScript.toCommandArguments(id, pid)
}

object ProcessConfiguration
{
  def forTest = ProcessConfiguration(
    encoding = if (isWindows) ISO_8859_1 else UTF_8)
}
