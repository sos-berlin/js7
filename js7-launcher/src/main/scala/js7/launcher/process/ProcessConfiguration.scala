package js7.launcher.process

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import js7.data.job.TaskId
import js7.launcher.forwindows.WindowsLogon

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  workingDirectory: Option[Path] = None,
  encoding: Charset,
  additionalEnvironment: Map[String, Option[String]] = Map.empty,
  maybeTaskId: Option[TaskId],
  windowsLogon: Option[WindowsLogon] = None)


object ProcessConfiguration:
  def forTest: ProcessConfiguration = ProcessConfiguration(
    encoding = UTF_8/*Windows ???*/,
    maybeTaskId = None)
