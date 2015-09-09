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
  fileOption: Option[Path] = None,
  idStringOption: Option[String] = None,
  killScriptFileOption: Option[Path] = None)
{
  require(killScriptFileOption.isEmpty || idStringOption.nonEmpty, "killScriptFile requires idString")

  for (id ← idStringOption) require(id.nonEmpty && id.trim == id, "Invalid ProcessConfiguration.idString")

  def files: immutable.Iterable[Path] = fileOption.toList ++ stdFileMap.values

  def idArgumentOption = idStringOption map { o ⇒ s"-agent-task-id=$o" }

  def killScriptArgument = s"-kill-agent-task-id=$idString"

  private def idString = idStringOption getOrElse { throw new IllegalArgumentException("Missing ProcessConfiguration.idString") }
}
