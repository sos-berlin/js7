package js7.executor.configuration

import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Path
import js7.base.io.file.FileUtils.WorkingDirectory
import js7.base.system.OperatingSystem.isWindows

final case class ExecutorConfiguration(
  jobWorkingDirectory: Path = WorkingDirectory,
  temporaryDirectory: Path,
  killScript: Option[ProcessKillScript])

object ExecutorConfiguration
{
  val FileEncoding = if (isWindows) ISO_8859_1 else UTF_8
}
