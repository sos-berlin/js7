package com.sos.jobscheduler.taskserver.task.process

import com.sos.jobscheduler.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.data.system.StdoutStderr.StdoutStderrType
import java.nio.file.Path
import scala.collection.immutable.Seq

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  stdFileMap: Map[StdoutStderrType, Path] = Map(),
  additionalEnvironment: Map[String, String] = Map(),
  agentTaskIdOption: Option[AgentTaskId] = None,
  killScriptOption: Option[ProcessKillScript] = None)
{
  require(killScriptOption.isEmpty || agentTaskIdOption.nonEmpty, "killScriptFile requires idString")

  for (id ← agentTaskIdOption) require(id.nonEmpty)

  def files: Seq[Path] = stdFileMap.values.toList

  def idArgumentOption = agentTaskIdOption map { o ⇒ s"-agent-task-id=${o.string}" }

  def toCommandArgumentsOption(pid: Option[Pid]) =
    for (id ← agentTaskIdOption; killScript ← killScriptOption) yield killScript.toCommandArguments(id, pid)
}
