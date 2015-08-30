package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import java.nio.file.Path
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  stdFileMap: Map[StdoutStderrType, Path] = Map(),
  additionalEnvironment: immutable.Iterable[(String, String)] = Map(),
  idString: String = "",
  killScriptFileOption: Option[Path] = None)
{
  def idArgumentOption = if (idString.nonEmpty) Some(s"-agent-task-id=$idString") else None

  def killScriptArgument = {
    requireIdString()
    s"-kill-agent-task-id=$idString"
  }

  private def requireIdString(): Unit = require(idString.nonEmpty, "Missing processConfiguration.idString")
}
