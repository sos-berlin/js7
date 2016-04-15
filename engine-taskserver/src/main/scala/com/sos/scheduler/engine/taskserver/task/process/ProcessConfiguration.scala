package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.scheduler.engine.common.process.Processes.Pid
import com.sos.scheduler.engine.common.process.StdoutStderr.StdoutStderrType
import java.nio.file.Path
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  stdFileMap: Map[StdoutStderrType, Path] = Map(),
  additionalEnvironment: Map[String, String] = Map(),
  fileOption: Option[Path] = None,
  agentTaskIdOption: Option[AgentTaskId] = None,
  killScriptOption: Option[ProcessKillScript] = None)
{
  require(killScriptOption.isEmpty || agentTaskIdOption.nonEmpty, "killScriptFile requires idString")

  for (id ← agentTaskIdOption) require(id.nonEmpty)

  def files: immutable.Iterable[Path] = fileOption.toList ++ stdFileMap.values

  def idArgumentOption = agentTaskIdOption map { o ⇒ s"-agent-task-id=${o.string}" }

  def toCommandArgumentsOption(pid: Option[Pid]) =
    for (id ← agentTaskIdOption; killScript ← killScriptOption) yield killScript.toCommandArguments(id, pid)
}
