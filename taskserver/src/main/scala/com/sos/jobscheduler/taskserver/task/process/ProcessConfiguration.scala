package com.sos.jobscheduler.taskserver.task.process

import com.sos.jobscheduler.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.system.StdoutOrStderr
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Path
import scala.collection.immutable.Seq

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  stdFileMap: Map[StdoutOrStderr, Path] = Map(),
  encoding: Charset,
  workingDirectory: Option[Path] = None,
  additionalEnvironment: Map[String, String] = Map(),
  agentTaskIdOption: Option[AgentTaskId] = None,
  killScriptOption: Option[ProcessKillScript] = None)
{
  require(killScriptOption.isEmpty || agentTaskIdOption.nonEmpty, "killScriptFile requires idString")

  for (id <- agentTaskIdOption) require(id.nonEmpty)

  def files: Seq[Path] = stdFileMap.values.toList

  def idArgumentOption = agentTaskIdOption map { o => s"-agent-task-id=${o.string}" }

  def toKillScriptCommandArgumentsOption(pid: Option[Pid]) =
    for (id <- agentTaskIdOption; killScript <- killScriptOption) yield killScript.toCommandArguments(id, pid)
}

object ProcessConfiguration
{
  def forTest = ProcessConfiguration(
    encoding = if (isWindows) ISO_8859_1 else UTF_8)
}
