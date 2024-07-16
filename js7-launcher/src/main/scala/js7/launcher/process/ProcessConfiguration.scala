package js7.launcher.process

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import js7.base.io.process.Pid
import js7.data.job.TaskId
import js7.launcher.configuration.ProcessKillScript
import js7.launcher.forwindows.WindowsLogon

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  workingDirectory: Option[Path] = None,
  encoding: Charset,
  additionalEnvironment: Map[String, Option[String]] = Map.empty,
  maybeTaskId: Option[TaskId],
  killWithSigterm: Seq[String],
  killWithSigkill: Seq[String],
  killForWindows: Seq[String],
  killScriptOption: Option[ProcessKillScript] = None,
  windowsLogon: Option[WindowsLogon] = None):

  require(killScriptOption.isEmpty || maybeTaskId.nonEmpty, "killScriptFile requires idString")

  for id <- maybeTaskId do require(id.nonEmpty)

  def idArgumentOption: Option[String] =
    maybeTaskId.map(o => s"--agent-task-id=${o.string}")

  def toKillScriptCommandArgumentsOption(pid: Option[Pid]): Option[Seq[String]] =
    for
      id <- maybeTaskId
      killScript <- killScriptOption
    yield killScript.toCommandArguments(id, pid)


object ProcessConfiguration:
  def forTest: ProcessConfiguration = ProcessConfiguration(
    encoding = UTF_8/*Windows ???*/,
    maybeTaskId = None,
    killWithSigterm = Seq("/bin/kill", "$pid"),
    killWithSigkill = Seq("/bin/kill", "-KILL", "$pid"),
    killForWindows = Seq("taskkill", "/pid", "$pid"))
